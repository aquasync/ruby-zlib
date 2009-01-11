
module Zlib
	class Deflate < ZStream
		# invalid hash _and_ invalid offset
		INVALID = -1

		# next & prev are array indices within the window
		WindowEntry = Struct.new(:next, :prev, :hashval)

		# first is window index of first in chain
		HashEntry = Struct.new(:first)

		Match = Struct.new(:distance, :len)

		WINSIZE    = 32768  # window size. Must be power of 2!
		HASHMAX    = 2039   # one more than max hash value
		MAXMATCH   = 32     # how many matches we track
		HASHCHARS  = 3      # how many chars make a hash

		CodeRecord = Struct.new(:code, :extra_bits, :range)

		LEN_CODES = [
			3,   4,   5,   6,   7,   8,   9,   10,
			11,  13,  15,  17,  19,  23,  27,  31,
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

		SYM_LIMIT             = 65536
		SYM_LITLEN            = 0x00000000
		SYM_DIST              = 0x40000000
		SYM_EXTRA_BITS        = 0x80000000
		SYM_CODE_LEN          = 0xC0000000
		SYM_MASK              = 0xC0000000
		SYM_EXTRA_BITS_MASK   = 0x3C000000
		SYM_EXTRA_BITS_SHIFT  = 26

		# Binary heap used by HuffmanTree.lengths_from_freqs
		#
		# Structured as an array of ints, with two ints per node
		# (user data and key pair).
		class BinaryHeap < Array
			def push userdata, key
				me = length
				super userdata, key
				while me > 0
					c = (me - 2) / 4 * 2 # parent
					break unless self[me + 1] < self[c + 1]
					self[me], self[c] = self[c], self[me]
					self[me + 1], self[c + 1] = self[c + 1], self[me + 1]
					me = c
				end
			end

			def shift
				len = length - 2
				pair = self[0], self[1]
				if len == 0
					clear
				else
					self[1] = pop
					self[0] = pop
				end
				me = 0
				loop do
					lc = me * 2 + 2 # left
					rc = me * 2 + 4 # right
					break if lc >= len
					c = rc >= len || self[lc + 1] < self[rc + 1] ? lc : rc
					break unless self[me + 1] > self[c + 1]
					self[me], self[c] = self[c], self[me]
					self[me + 1], self[c + 1] = self[c + 1], self[me + 1]
					me = c
				end
				pair
			end
		end

		class HuffmanTree
			HUFF_MAX = 286
			MAX_CODE_LEN = 16

			attr_reader :codes, :lengths, :freqs

			def initialize params={}
				if freqs = params[:freqs]
					lengths = self.class.lengths_from_freqs_limited freqs.dup, params[:limit] || 15
				end
				if lengths ||= params[:lengths]
					codes = self.class.codes_from_lengths(lengths).last
				end
				unless codes ||= params[:codes]
					raise ArgumentError, 'must specify either :freqs, :lengths, or :codes'
				end
				@codes = codes
				@lengths = lengths
				@freqs = freqs
			end

			def self.codes_from_lengths lengths
				codes = [nil] * lengths.length
				count = [0] * MAX_CODE_LEN
				start_code = count.dup

				# Count the codes of each length.
				lengths.each { |l| count[l] += 1 }
				max_len = lengths.max

				# Determine the starting code for each length block.
				code = 0
				(1...MAX_CODE_LEN).each do |i|
					start_code[i] = code
					code += count[i]
					# overcommitted
					max_len = -1 if code > (1 << i)
					code <<= 1
				end
				# undercommitted
				max_len = -2 if code < (1 << MAX_CODE_LEN)

				# Determine the code for each symbol. Mirrored, of course.
				lengths.length.times do |i|
					code = start_code[lengths[i]]
					start_code[lengths[i]] += 1
					codes[i] = 0
					lengths[i].times do |j|
						codes[i] = (codes[i] << 1) | (code & 1)
						code >>= 1
					end
				end

				[max_len, codes]
			end

			# The core of the Huffman algorithm: takes an input array of
			# symbol frequencies, and produces an output array of code
			# lengths.
			#
			# This is basically a generic Huffman implementation, but it has
			# one zlib-related quirk which is that it caps the output code
			# lengths to fit in an unsigned char (which is safe since Deflate
			# will reject anything longer than 15 anyway). Anyone wanting to
			# rip it out and use it in another context should find that easy
			# to remove.
			def self.lengths_from_freqs freqs
				nsyms  = freqs.length
				parent = [0] * (2 * HUFF_MAX - 1)
				length = [0] * (2 * HUFF_MAX - 1)
				heap   = BinaryHeap.new

				# Begin by building the heap, leaving out unused symbols entirely
				nsyms.times { |i| heap.push i, freqs[i] if freqs[i] > 0 }

				# Now repeatedly take two elements off the heap and merge them.
				n = HUFF_MAX
				while heap.length > 2
					i, si = heap.shift
					j, sj = heap.shift
					parent[i] = parent[j] = n
					heap.push n, si + sj
					n += 1
				end

				# Now we have our tree, in the form of a link from each node
				# to the index of its parent. Count back down the tree to
				# determine the code lengths.

				# The tree root has length 0 after that, which is correct.
				(n - 1).downto(0) do |i|
					length[i] = 1 + length[parent[i]] if parent[i] > 0
				end

				# And that's it. (Simple, wasn't it?) Copy the lengths into
				# the output array and leave.
				#
				# Here we cap lengths to fit in unsigned char.
				(0...nsyms).map { |i| [length[i], 255].min }
			end

			# Wrapper around lengths_from_freqs which enforces the Deflate
			# restriction that no code length may exceed 15 bits, or 7 for the
			# auxiliary length alphabet. This function has the same calling
			# semantics as lengths_from_freqs, except that it might modify the
			# freqs array.
			def self.lengths_from_freqs_limited(freqs, limit)
				# Nasty special case: if the frequency table has fewer than
				# two non-zero elements, we must invent some, because we can't
				# have fewer than one bit encoding a symbol.
				# Is this even possible? maybe for an empty block, where all you
				# encode is the EOB marker.
				count = freqs.inject(0) { |a, b| b > 0 ? a + 1 : a }
				count.times { freqs[freqs.index(0)] = 1 } if count < 2

				# First, try building the Huffman table the normal way. If
				# this works, it's optimal, so we don't want to mess with it.
				lengths = lengths_from_freqs(freqs)
				return lengths if lengths.all? { |l| l <= limit } # OK

				# FIXME ... (Massive comment elided at this point) ...

				maxprob = limit == 15 ? 2584 : 55
				temp = freqs.reject { |freq| freq == 0 }
				nactivesyms = temp.length
				smallestfreq = temp.min
				totalfreq = temp.inject { |a, b| a + b }

				# We want to find the smallest integer `adjust' such that
				# (totalfreq + nactivesyms * adjust) / (smallestfreq +
				# adjust) is less than maxprob. A bit of algebra tells us
				# that the threshold value is equal to
				#
				#   totalfreq - maxprob * smallestfreq
				#   ----------------------------------
				#          maxprob - nactivesyms
				#
				# rounded up, of course. And we'll only even be trying
				# this if
				num = totalfreq - smallestfreq * maxprob
				denom = maxprob - nactivesyms
				adjust = (num + denom - 1) / denom

				# Now add `adjust' to all the input symbol frequencies and rebuild
				# trees.
				freqs.map! { |freq| freq == 0 ? 0 : freq + adjust }
				lengths_from_freqs(freqs)
			end

			def self.static_literal_length
				@static_literal_lengths ||=
					new(:lengths => [8] * 144 + [9] * 112 + [7] * 24 + [8] * 8)
			end

			def self.static_distance
				@static_distance ||= new(:lengths => [5] * 30)
			end
		end

		def self.deflate data, level=DEFAULT_COMPRESSION
			new(level).deflate data, FINISH
		end

		# note that we currently ignore all of the parameters, except level
		def initialize level=DEFAULT_COMPRESSION, windowBits=MAX_WBITS, memlevel=nil, strategy=DEFAULT_STRATEGY
			@zstring = ''
			@header = false
			@output = BitWriter.new StringIO.new

			# LZ77 stuff
			# this seems a bit wasteful
			@win = (0...WINSIZE).map { WindowEntry.new INVALID, INVALID, INVALID }
			@hashtab = (0...HASHMAX).map { HashEntry.new INVALID }
			@winpos = 0
			@pending = 0.chr * HASHCHARS
			@npending = 0
			@data = 0.chr * WINSIZE

			# DEFLATE stuff
			@nsyms = 0
			@syms = [nil] * SYM_LIMIT
			@symstart = 0
			@level = level
			@lastblock = false
		end

		def output
			@output.io.string
		end

		def set_dictionary dict
			@dict = dict
		end

		def params level, strategy
			if level != @level
				deflate '', NO_FLUSH
				flushblock
				@level = level
				# this pointless flushblock lets us get same bytestream as zlib.
				# its not a sync flush, so i'm not sure why its done. waste of
				# bits...
				flushblock
			end
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
			if @win[@winpos].prev != INVALID
				@win[@win[@winpos].prev].next = INVALID
			elsif @win[@winpos].hashval != INVALID
				@hashtab[@win[@winpos].hashval].first = INVALID
			end

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

				level_flags = @level == NO_COMPRESSION ? 0 : 2

				header = ((DEFLATED + ((MAX_WBITS - 8) << 4)) << 8) | (level_flags << 6)
				header |= PRESET_DICT if @dict
				header += 31 - (header % 31)

				outbits header >> 8, 8
				outbits header & 0xff, 8

				if @dict and data.length >= 3 # HASHCHARS i suppose
					dict = @dict + data[0, 3]
					@dict.length.times do |i|
						lz77_advance dict[0], lz77_hash(dict)
						dict.slice! 0, 1
					end
					outbits [Checksum.adler32(@dict)].pack('V').unpack('N')[0], 32
				end
			end

			unless @zstring.empty?
				data = @zstring + data
				@zstring = ''
			end

			# NOTE this is currently destructive to the data (uses slice!)
			# should probably be fixed...
			lz77_compress data.dup, @level != NO_COMPRESSION

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
				# Close the current block.
				flushblock

				# Then output an empty _uncompressed_ block: send 000,
				# then sync to byte boundary, then send bytes 00 00 FF
				# FF.
				outbits 0, 3
				pending = @output.instance_variable_get(:@have)
				outbits 0, 8 - pending if pending != 0
				outbits 0, 16
				outbits 0xffff, 16
				''
			when FULL_FLUSH
				# not sure how this differs from the above...
				raise NotImplementedError
			when FINISH
				@lastblock = true
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

		def literal c
			outsym SYM_LITLEN | c
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
				outsym SYM_LITLEN | code.code

				# Transmit the extra bits.
				if code.extra_bits > 0
					outsym SYM_EXTRA_BITS | (thislen - code.range.begin) |
						(code.extra_bits << SYM_EXTRA_BITS_SHIFT)
				end

				# Binary-search to find which distance code we're
				# transmitting.
				# NOTE: no longer binary search atm. this is slower
				code = DIST_CODES.find { |c| c.range === distance }

				# Write the distance code.
				outsym SYM_DIST | code.code

				# Transmit the extra bits.
				if code.extra_bits > 0
					outsym SYM_EXTRA_BITS | (distance - code.range.begin) |
						(code.extra_bits << SYM_EXTRA_BITS_SHIFT)
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
			basesym = sym & ~SYM_MASK
			litlen, dist, codelen = huftrees

			case sym & SYM_MASK
			when SYM_LITLEN
				outbits litlen.codes[basesym], litlen.lengths[basesym]
			when SYM_DIST
				outbits dist.codes[basesym], dist.lengths[basesym]
			when SYM_CODE_LEN
				outbits codelen.codes[basesym], codelen.lengths[basesym]
			when SYM_EXTRA_BITS
				outbits basesym & ~SYM_EXTRA_BITS_MASK, basesym >> SYM_EXTRA_BITS_SHIFT
			end
		end

		def outsym sym
			@syms[(@symstart + @nsyms) % SYM_LIMIT] = sym
			@nsyms += 1
			chooseblock if @nsyms == SYM_LIMIT
		end

		def symsize sym, huftrees
			basesym = sym & ~SYM_MASK
			litlen, dist, codelen = huftrees

			case sym & SYM_MASK
			when SYM_LITLEN
				litlen.lengths[basesym]
			when SYM_DIST
				dist.lengths[basesym]
			when SYM_CODE_LEN
				codelen.lengths[basesym]
			when SYM_EXTRA_BITS
				basesym >> SYM_EXTRA_BITS_SHIFT
			end
		end

		# outblock() must output _either_ a dynamic block of length
		# `dynamic_len', _or_ a static block of length `static_len', but
		# it gets to choose which.
		def outblock dynamic_len, static_len
			if @level == NO_COMPRESSION
				bfinal = @lastblock ? 1 : 0
				outbits bfinal, 1
				outbits 0, 3
				blklen = static_len # can't think why this wouldn't be equal to @nsyms
				pending = @output.instance_variable_get(:@have)
				outbits 0, 8 - pending if pending != 0
				outbits blklen & 0xff, 8
				outbits((blklen >> 8) & 0xff, 8)
				outbits ~blklen & 0xff, 8
				outbits((~blklen >> 8) & 0xff, 8)
				blklen.times do |i|
					sym = @syms[(@symstart + i) % SYM_LIMIT]
					@output.io << sym.chr
				end

				# Remove all the just-output symbols from the symbol buffer by
				# adjusting symstart and nsyms.
				@symstart = (@symstart + blklen) % SYM_LIMIT
				@nsyms -= blklen

				return
			end

			# We make our choice of block to output by doing all the
			# detailed work to determine the exact length of each possible
			# block. Then we choose the one which has fewest output bits
			# per symbol.

			# First build the two main Huffman trees for the dynamic
			# block.

			# Count up the frequency tables.
			freqs1 = [0] * 286
			freqs2 = [0] * 30
			freqs1[256] = 1          # we're bound to need one EOB

			dynamic_len.times do |i|
				sym = @syms[(@symstart + i) % SYM_LIMIT]

				# Increment the occurrence counter for this symbol, if
				# it's in one of the Huffman alphabets and isn't extra
				# bits.
				if (sym & SYM_MASK) == SYM_LITLEN
					sym &= ~SYM_MASK;
					freqs1[sym] += 1
				elsif (sym & SYM_MASK) == SYM_DIST
					sym &= ~SYM_MASK
					freqs2[sym] += 1
				end
			end

			litlen = HuffmanTree.new :freqs => freqs1
			dist = HuffmanTree.new :freqs => freqs2

			# Determine HLIT and HDIST.
			hlit = 286
			hlit -= 1 while hlit > 257 and litlen.lengths[hlit - 1] == 0
			hdist = 30
			hdist -= 1 while hdist > 1 and dist.lengths[hdist - 1] == 0

			# Write out the list of symbols used to transmit the
			# trees.
			treesrc = []
			hlit.times { |i|  treesrc << litlen.lengths[i] }
			hdist.times { |i| treesrc << dist.lengths[i] }

			treesyms = []
			i = 0
			while i < treesrc.length
				# Find length of run of the same length code.
				j = 1
				j += 1 while treesrc[i + j] == treesrc[i]

				# Encode that run as economically as we can.
				k = j
				if treesrc[i] == 0
					# Zero code length: we can output run codes for
					# 3-138 zeroes. So if we have fewer than 3 zeroes,
					# we just output literals. Otherwise, we output
					# nothing but run codes, and tweak their lengths
					# to make sure we aren't left with under 3 at the
					# end.
					if k < 3
						k.times { treesyms.push 0 | SYM_CODE_LEN }
					else
						while k > 0
							rpt = k > 140 ? 138 : k <= 138 ? k : k - 3
							if rpt < 11
								treesyms.push 17 | SYM_CODE_LEN;
								treesyms.push SYM_EXTRA_BITS | (rpt - 3) |
									(3 << SYM_EXTRA_BITS_SHIFT)
							else
								treesyms.push 18 | SYM_CODE_LEN;
								treesyms.push SYM_EXTRA_BITS | (rpt - 11) |
									(7 << SYM_EXTRA_BITS_SHIFT)
							end
							k -= rpt
						end
					end
				else
					# Non-zero code length: we must output the first
					# one explicitly, then we can output a copy code
					# for 3-6 repeats. So if we have fewer than 4
					# repeats, we _just_ output literals. Otherwise,
					# we output one literal plus at least one copy
					# code, and tweak the copy codes to make sure we
					# aren't left with under 3 at the end.
					treesyms.push treesrc[i] | SYM_CODE_LEN
					k -= 1
					if k < 3
						k.times { treesyms.push treesrc[i] | SYM_CODE_LEN }
					else
						while k > 0
							rpt = [k, 6].min
							rpt = k - 3 if (rpt > k - 3 && rpt < k)
							treesyms.push 16 | SYM_CODE_LEN;
							treesyms.push SYM_EXTRA_BITS | (rpt - 3) |
								(2 << SYM_EXTRA_BITS_SHIFT)
							k -= rpt
						end
					end
				end

				i += j
			end

			# Count up the frequency table for the tree-transmission
			# symbols, and build the auxiliary Huffman tree for that.
			freqs3 = [0] * 19
			treesyms.each do |sym|
				# Increment the occurrence counter for this symbol, if
				# it's the Huffman alphabet and isn't extra bits.
				freqs3[sym & ~SYM_MASK] += 1 if (sym & SYM_MASK) == SYM_CODE_LEN
			end

			codelen = HuffmanTree.new :freqs => freqs3, :limit => 7

			lenlenmap = [16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15]

			# Reorder the code length codes into transmission order, and
			# determine HCLEN.
			trans = (0...19).map { |i| codelen.lengths[lenlenmap[i]] }
			hclen = 19
			hclen -= 1 while hclen > 4 and trans[hclen - 1] == 0

			# Now work out the exact size of both the dynamic and the
			# static block, in bits.

			dht = [litlen, dist, codelen]
			sht = [HuffmanTree.static_literal_length, HuffmanTree.static_distance, nil]

			# First the dynamic block.
			dsize = 3 + 5 + 5 + 4     # 3-bit header, HLIT, HDIST, HCLEN
			dsize += 3 * hclen        # code-length-alphabet code lengths
			# Code lengths
			treesyms.each { |sym| dsize += symsize(sym, dht) }
			# The actual block data
			dynamic_len.times do |i|
				sym = @syms[(@symstart + i) % SYM_LIMIT]
				dsize += symsize(sym, dht)
			end
			# And the end-of-data symbol.
			dsize += symsize(SYM_LITLEN | 256, dht)

			# Now the static block.
			ssize = 3          # 3-bit block header
			# The actual block data
			static_len.times do |i|
				sym = @syms[(@symstart + i) % SYM_LIMIT]
				ssize += symsize(sym, sht)
			end
			# And the end-of-data symbol.
			ssize += symsize(SYM_LITLEN | 256, sht);

			# Compare the two and decide which to output. We break
			# exact ties in favour of the static block, because of the
			# special case in which that block has zero length.
			dynamic = ssize * dynamic_len > dsize * static_len

			# 3-bit block header
			bfinal = @lastblock ? 1 : 0
			btype = dynamic ? 2 : 1
			outbits bfinal, 1
			outbits btype, 2

			# Actually transmit the block.

			if dynamic
				# HLIT, HDIST and HCLEN
				outbits hlit - 257, 5
				outbits hdist - 1, 5
				outbits hclen - 4, 4

				# Code lengths for the auxiliary tree
				hclen.times { |i| outbits trans[i], 3 }

				# Code lengths for the literal/length and distance trees
				treesyms.each { |sym| writesym sym, dht }

				blklen, ht = dynamic_len, dht
			else
				blklen, ht = static_len, sht
			end

			# Output the actual symbols from the buffer
			blklen.times do |i|
				sym = @syms[(@symstart + i) % SYM_LIMIT]
				writesym sym, ht
			end

			# Output the end-of-data symbol
			writesym SYM_LITLEN | 256, ht

			# Remove all the just-output symbols from the symbol buffer by
			# adjusting symstart and nsyms.
			@symstart = (@symstart + blklen) % SYM_LIMIT
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
				sym = @syms[(@symstart + i) % SYM_LIMIT]

				if i > 0 && (sym & SYM_MASK) == SYM_LITLEN
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
				if (sym & SYM_MASK) == SYM_LITLEN
					sym &= ~SYM_MASK
					len += freqs1[sym] * 8 * log2(freqs1[sym])
					len -= total1 * approxlog2(total1)
					freqs1[sym] += 1
					total1 += 1
					len -= freqs1[sym] * 8 * log2(freqs1[sym])
					len += total1 * 8 * log2(total1)
				elsif (sym & SYM_MASK) == SYM_DIST
					sym &= ~SYM_MASK
					len += freqs2[sym] * 8 * log2(freqs2[sym])
					len -= total2 * 8 * log2(total2)
					freqs2[sym] += 1
					total2 += 1
					len -= freqs2[sym] * 8 * log2(freqs2[sym])
					len += total2 * 8 * log2(total2)
				elsif (sym & SYM_MASK) == SYM_EXTRA_BITS
					len += 8 * ((sym &~ SYM_MASK) >> SYM_EXTRA_BITS_SHIFT)
				end
			end

			outblock bestlen, longestlen
		end

		def inspect
			# hide some of our massive variables.
			vals = (instance_variables - %w[@data @win @syms @hashtab]).map do |iv|
				" #{iv}=#{instance_variable_get(iv).inspect}"
			end
			"#{to_s[0..-2]}#{vals.join(',')}>"
		end
	end
end

