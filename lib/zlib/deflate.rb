
module Zlib
	class Deflate < ZStream
		# invalid hash _and_ invalid offset
		INVALID = -1

		# next & prev are array indices within the window
		WindowEntry = Struct.new(:next, :prev, :hashval)

		# first is window index of first in chain
		HashEntry = Struct.new(:first)

		Match = Struct.new(:distance, :len)

		WINSIZE    = 32768	# window size. Must be power of 2!
		HASHMAX    = 2039		# one more than max hash value
		MAXMATCH   = 32		  # how many matches we track
		HASHCHARS  = 3		  # how many chars make a hash
		MAXCODELEN = 16

		CodeRecord = Struct.new(:code, :extra_bits, :range)

		LEN_CODES = [
			3,   4,   5,   6,   7,   8,   9,   10,
			11,  13,  15,  17,  19,  23, 	27,  31, 
			35,  43,  51,  59,  67,  83,  99,  115,
			131, 163, 195, 227, 258
		].push(259).enum_with_index.to_enum(:each_cons, 2).map do |(l1, i), (l2, j)|
			CodeRecord.new i + 257, i == 28 ? 0 : [i / 4 - 1, 0].max, l1...l2
		end

		DIST_CODES = [
			1,     2,     3,     4,     5,     7,     9,     13,
			17,    25,    33,    49,    65,    97,    129,   193,
			257,   385,   513,   769,   1025,  1537,  2049,  3073,
			4097,  6145,  8193,  12289, 16385, 24577
		].push(32769).enum_with_index.to_enum(:each_cons, 2).map do |(l1, i), (l2, j)|
			CodeRecord.new i, [i / 2 - 1, 0].max, l1...l2
		end

		# not quite sure of the meaning of these yet...
		SYMLIMIT              = 65536
		SYMPFX_LITLEN         = 0x00000000
		SYMPFX_DIST           = 0x40000000
		SYMPFX_EXTRABITS      = 0x80000000
		SYMPFX_CODELEN        = 0xC0000000
		SYMPFX_MASK           = 0xC0000000
		SYM_EXTRABITS_MASK    = 0x3C000000
		SYM_EXTRABITS_SHIFT   = 26

		class HuffmanTree
			attr_reader :codes, :lengths
			def initialize
				@codes = []
				@lengths = []
			end
		end
		
		def self.deflate data, level=DEFAULT_COMPRESSION
			new(level).deflate data, FINISH
		end

		# note that we currently ignore all of the parameters...
		def initialize level=DEFAULT_COMPRESSION, windowBits=MAX_WBITS, memlevel=nil, strategy=DEFAULT_STRATEGY
			@zstring = ''
			@header = false

			# LZ77 stuff
			# -----

			# this is fairly memory consuming. 
			@win = (0...WINSIZE).map { WindowEntry.new INVALID, INVALID, INVALID }
			@hashtab = (0...HASHMAX).map { HashEntry.new INVALID }
			@winpos = 0
			@pending = 0.chr * HASHCHARS
			@npending = 0
			@data = 0.chr * WINSIZE

			# DEFLATE stuff
			# -----

			@nsyms = 0
			@syms = [nil] * SYMLIMIT
			@symstart = 0

			# Huffman stuff
			# -----

			lengths = [8] * 144 + [9] * 112 + [7] * 24 + [8] * 8
			maxlen, codes = hufcodes lengths

#			codes.zip(lengths).each_with_index do |(code, length), i|
#				p i => [code].pack('N').unpack('B*')[0].reverse.match(/(#{'.' * length})(.*)/)[1..-1].join('|')
#			end

			@static_litlen = HuffmanTree.new
			@static_litlen.codes.replace codes
			@static_litlen.lengths.replace lengths

			lengths = [5] * 30
			maxlen, codes = hufcodes lengths

#			codes.zip(lengths).each_with_index do |(code, length), i|
#				p i => [code].pack('N').unpack('B*')[0].reverse.match(/(#{'.' * length})(.*)/)[1..-1].join('|')
#			end

			@static_dist = HuffmanTree.new
			@static_dist.codes.replace codes
			@static_dist.lengths.replace lengths
			
			@output = BitWriter.new StringIO.new
		end
		
		def output
			@output.io.string
		end
		
		def set_dictionary dict
			@dict = dict
		end
		
		def params(*ignore)
		end

		def << data
			@zstring << data
		end

		def finish
			deflate '', FINISH
			output
		end

		def lz77_hash data
			(257 * data[0] + 263 * data[1] + 269 * data[2]) % HASHMAX
		end

		def lz77_advance c, hash
			# Remove the hash entry at winpos from the tail of its chain,
			# or empty the chain if it's the only thing on the chain.
#			if @win[@winpos].prev != INVALID
#				@win[@win[@winpos].prev].next = INVALID
#			elsif @win[@winpos].hashval != INVALID
#				@hashtab[@win[@winpos].hashval].first = INVALID
#			end

			# Create a new entry at winpos and add it to the head of its
			# hash chain.
			@win[@winpos].hashval = hash
			@win[@winpos].prev = INVALID
			off = @win[@winpos].next = @hashtab[hash].first
			@hashtab[hash].first = @winpos
			if off != INVALID
				@win[off].prev = @winpos
			end
			@data[@winpos] = c

			# Advance the window pointer.
			@winpos = (@winpos + 1) & (WINSIZE - 1)
		end

		def lz77_compress data, compress=true
			len = data.length
			
			charat = proc do |k|
				k < 0 ? @data[(@winpos + k) & (WINSIZE - 1)] : data[k]
			end

			# Add any pending characters from last time to the window. (We
			# might not be able to.)
			i = 0
			temp = 0.chr * HASHCHARS
			@npending.times do |i| # we purposefully shadow the outer i
				if len + @npending - i < HASHCHARS
					# Update the pending array.
					(i...@npending).each { |j| @pending[j - i] = @pending[j] }
					break
				end
				(0...HASHCHARS).each do |j|
					temp[j] = i + j < @npending ? @pending[i + j] :
						data[i + j - @npending]
				end
				lz77_advance temp[0], lz77_hash(temp)
			end
			@npending -= i

		  matches = []
		  defermatch = Match.new 0, 0
			deferchr = 0
			advance = 0

			while len > 0
				matches.clear

				# Don't even look for a match, if we're not compressing.
				if compress && len >= HASHCHARS
					# Hash the next few characters.
					hash = lz77_hash data
					# Look the hash up in the corresponding hash chain and see
					# what we can find.
					off = @hashtab[hash].first
					while off != INVALID
						# distance = 1       if off == @winpos-1
						# distance = WINSIZE if off == @winpos
						distance = WINSIZE - (off + WINSIZE - @winpos) % WINSIZE
						found = HASHCHARS.times do |i|
							break true if charat[i] != charat[i - distance]
						end
						if found != true
							matches << Match.new(distance, 3)
							break if matches.length >= MAXMATCH
						end
						off = @win[off].next
					end
				else
					hash = INVALID
				end

				if matches.empty?
					# We found no matches. Emit the deferred match, if
					# any; otherwise emit a literal.
					if defermatch.len > 0
						self.match defermatch.distance, defermatch.len
						advance = defermatch.len - 1
						defermatch.len = 0
					else
						self.literal data[0]
						advance = 1
					end
				else
					# We've now filled up matches[] with nmatch potential
					# matches. Follow them down to find the longest. (We
					# assume here that it's always worth favouring a
					# longer match over a shorter one.)
					matchlen = HASHCHARS
					while matchlen < len
						still_matching = matches.select do |match|
							charat[matchlen] == charat[matchlen - match.distance]
						end
						break if still_matching.empty?
						matches = still_matching
						matchlen += 1
					end

					# We've now got all the longest matches. We favour the
					# shorter distances, which means we go with matches[0].
					# So see if we want to defer it or throw it away.
					matches[0].len = matchlen
					if defermatch.len > 0
						if matches[0].len > defermatch.len + 1
							# We have a better match. Emit the deferred char,
							# and defer this match.
							self.literal deferchr
							defermatch = matches[0]
							deferchr = data[0]
							advance = 1
						else
							# We don't have a better match. Do the deferred one.
							self.match defermatch.distance, defermatch.len
							advance = defermatch.len - 1
							defermatch.len = 0
						end
					else
						# There was no deferred match. Defer this one.
						defermatch = matches[0]
						deferchr = data[0]
						advance = 1
					end
				end

			# Now advance the position by `advance' characters,
			# keeping the window and hash chains consistent.
			while advance > 0
				if len >= HASHCHARS
					lz77_advance data[0], lz77_hash(data)
				else
					@pending[@npending] = data[0]
					@npending += 1
				end
				# no pointers. this may be very inefficient, but don't care for now
				data.slice! 0, 1
				len     -= 1
				advance -= 1
			end
			end
		end

		# no idea what the flush default is supposed to be, so none provided here...
		def deflate data, flush
			unless @header
				@header = true

				# FIXME
				level_flags = 2
	
				header = ((DEFLATED + ((MAX_WBITS - 8) << 4)) << 8) | (level_flags << 6)
				header |= PRESET_DICT if @dict
				header += 31 - (header % 31)
				
				outbits header >> 8, 8
				outbits header & 0xff, 8
	
				unless @zstring.empty?
					data = @zstring + data
					@zstring = ''
				end

				if @dict and data.length >= 3 # HASHCHARS i suppose
					dict = @dict + data[0, 3]
					@dict.length.times do |i|
						lz77_advance dict[0], lz77_hash(dict)
						dict.slice! 0, 1
					end
					outbits [Checksum.adler32(@dict)].pack('V').unpack('N')[0], 32
				end
			end

			# NOTE this is currently destructive to the data (uses slice!)
			# should probably be fixed...
			lz77_compress data.dup
			
			# Update checksum
			# This is for Zlib. Gzip uses a CRC32 instead
			# Just write out the Adler32 checksum.
			@checksum ||= 1
			@checksum = Checksum.adler32(data, @checksum)

			case flush
			when NO_FLUSH
				# note that even with no flush, CZlib will return the header on the
				# first go... could maybe try to replicate that but it seems weird
				''
			when PARTIAL_FLUSH, SYNC_FLUSH
				raise NotImplementedError
			when FULL_FLUSH
				raise NotImplementedError
			when FINISH
				flushblock

				# Sync to byte boundary, flushing out the final byte.
				pending = @output.instance_variable_get(:@have)
				outbits 0, 8 - pending if pending != 0

				# Format-specific trailer data.
				outbits [@checksum].pack('V').unpack('N')[0], 32

				output
			when BLOCK
				raise NotImplementedError
			else
				raise ArgumentError, 'bad flush value - %p' % flush
			end
		end

		def hufcodes lengths
			codes = [nil] * lengths.length
			count = [0] * MAXCODELEN
			startcode = count.dup

		  # Count the codes of each length.
			lengths.each { |l| count[l] += 1 }
			maxlen = lengths.max

			# Determine the starting code for each length block.
			code = 0
			(1...MAXCODELEN).each do |i|
				startcode[i] = code
				code += count[i]
				# overcommitted
				maxlen = -1 if code > (1 << i)
				code <<= 1
			end
			# undercommitted
			maxlen = -2 if code < (1 << MAXCODELEN)

		  # Determine the code for each symbol. Mirrored, of course.
		  lengths.length.times do |i|
				code = startcode[lengths[i]]
				startcode[lengths[i]] += 1
				codes[i] = 0
				lengths[i].times do |j|
					codes[i] = (codes[i] << 1) | (code & 1)
					code >>= 1
				end
		  end

			[maxlen, codes]
		end
		
		def literal c
			outsym SYMPFX_LITLEN | c
		end

		def match distance, len
			while len > 0
				# We can transmit matches of lengths 3 through 258
				# inclusive. So if len exceeds 258, we must transmit in
				# several steps, with 258 or less in each step.
				#
				# Specifically: if len >= 261, we can transmit 258 and be
				# sure of having at least 3 left for the next step. And if
				# len <= 258, we can just transmit len. But if len == 259
				# or 260, we must transmit len-3.
				thislen = len > 260 ? 258 : len <= 258 ? len : len - 3
				len -= thislen
		
				# Binary-search to find which length code we're
				# transmitting.
				# NOTE: no longer binary search atm. this is slower
				code = LEN_CODES.find { |c| c.range === thislen }

				# Transmit the length code.
				outsym SYMPFX_LITLEN | code.code
		
				# Transmit the extra bits.
				if code.extra_bits > 0
					outsym SYMPFX_EXTRABITS | (thislen - code.range.begin) |
						(code.extra_bits << SYM_EXTRABITS_SHIFT)
				end
		
				# Binary-search to find which distance code we're
				# transmitting.
				# NOTE: no longer binary search atm. this is slower
				code = DIST_CODES.find { |c| c.range === distance }
		
				# Write the distance code.
				outsym SYMPFX_DIST | code.code
		
				# Transmit the extra bits.
				if code.extra_bits > 0
					outsym SYMPFX_EXTRABITS | (distance - code.range.begin) |
						(code.extra_bits << SYM_EXTRABITS_SHIFT)
				end
			end
		end

		def flushblock
			outblock @nsyms, @nsyms
		end

		def outbits val, count
			@output.write val, count
		end

		# Write out a single symbol, given the three Huffman trees.
		def writesym sym, huftrees
			basesym = sym &~ SYMPFX_MASK
			litlen, dist, codelen = huftrees
		
			case sym & SYMPFX_MASK
			when SYMPFX_LITLEN
#				p :litlen => basesym
				outbits litlen.codes[basesym], litlen.lengths[basesym]
		  when SYMPFX_DIST
#				p :dist => basesym
				outbits dist.codes[basesym], dist.lengths[basesym]
		  when SYMPFX_CODELEN
#				p :codelen => basesym
				outbits codelen.codes[basesym], codelen.lengths[basesym]
		  when SYMPFX_EXTRABITS
				i = basesym >> SYM_EXTRABITS_SHIFT
				basesym &= ~SYM_EXTRABITS_MASK
#				p :extrabits => [basesym, i]
				outbits basesym, i
			end
		end

		def outsym sym
			@syms[(@symstart + @nsyms) % SYMLIMIT] = sym
			@nsyms += 1
			chooseblock if @nsyms == SYMLIMIT
		end

		# outblock() must output _either_ a dynamic block of length
		# `dynamic_len', _or_ a static block of length `static_len', but
		# it gets to choose which.
		def outblock dynamic_len, static_len
			#p :outblock => [dynamic_len, static_len]
			#p @syms[0, static_len]

			# ...
			@lastblock = true
			dynamic = false
			blklen = static_len
			ht = [@static_litlen, @static_dist, nil]
			#warn 'dynamic blocks not implemented'

			# Actually transmit the block.

			# 3-bit block header
			bfinal = @lastblock ? 1 : 0
			btype = dynamic ? 2 : 1
			outbits bfinal, 1
			outbits btype, 2

			if dynamic
				# ...
				raise NotImplementedError
			end

			# Output the actual symbols from the buffer
			blklen.times do |i|
				sym = @syms[(@symstart + i) % SYMLIMIT]
				writesym sym, ht
			end

			# Output the end-of-data symbol
			writesym SYMPFX_LITLEN | 256, ht

			# Remove all the just-output symbols from the symbol buffer by
			# adjusting symstart and nsyms.
			@symstart = (@symstart + blklen) % SYMLIMIT
			@nsyms -= blklen
		end

		LOG_2 = Math.log(2)

		def log2(x)
			(Math.log(x) / LOG_2).ceil * 8
		end

		# the point of what this is doing is not clear to me yet
		def chooseblock
			freqs1 = [0] * 286
			freqs2 = [0] * 30

			freqs1[256] = 1 # we're bound to need one EOB
			longestlen = 0
			total1 = 1
			total2 = 0

			# Iterate over all possible block lengths, computing the
			# entropic coding approximation to the final length at every
			# stage. We divide the result by the number of symbols
			# encoded, to determine the `value for money' (overall
			# bits-per-symbol count) of a block of that length.
			bestlen = -1
			bestvfm = 0

			len = 300 * 8 # very approximate size of the Huffman trees
		
			@nsyms.times do |i|
				sym = @syms[(@symstart + i) % SYMLIMIT]
		
				if i > 0 && (sym & SYMPFX_MASK) == SYMPFX_LITLEN
					# This is a viable point at which to end the block.
					# Compute the value for money.
					vfm = i * 32768 / len      # symbols encoded per bit
					if bestlen < 0 || vfm > bestvfm
						bestlen = i
						bestvfm = vfm
					end
					longestlen = i
				end
		
				# Increment the occurrence counter for this symbol, if
				# it's in one of the Huffman alphabets and isn't extra
				# bits.
				if (sym & SYMPFX_MASK) == SYMPFX_LITLEN
					sym &= ~SYMPFX_MASK
					len += freqs1[sym] * 8 * log2(freqs1[sym])
					len -= total1 * approxlog2(total1)
					freqs1[sym] += 1
					total1 += 1
					len -= freqs1[sym] * 8 * log2(freqs1[sym])
					len += total1 * 8 * log2(total1)
				elsif (sym & SYMPFX_MASK) == SYMPFX_DIST
					sym &= ~SYMPFX_MASK
					len += freqs2[sym] * 8 * log2(freqs2[sym])
					len -= total2 * 8 * log2(total2)
					freqs2[sym] += 1
					total2 += 1
					len -= freqs2[sym] * 8 * log2(freqs2[sym])
					len += total2 * 8 * log2(total2)
				elsif (sym & SYMPFX_MASK) == SYMPFX_EXTRABITS
					len += 8 * ((sym &~ SYMPFX_MASK) >> SYM_EXTRABITS_SHIFT)
				end
			end
		
			outblock bestlen, longestlen
		end
	end
end

