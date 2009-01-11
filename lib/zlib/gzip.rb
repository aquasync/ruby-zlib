module Zlib
	class GzipFile
		class Error < Exception
		end

		def initialize
			@input_buffer = []
			@output_buffer = []
			@out_pos = -1
			@in_pos = -1
			@closed = false

			@orig_name = nil
			@comment = nil
		end

		def close
			finish
			@io.close if @io.respond_to?(:close)
		end

		def closed?
			@closed
		end
		
		def finish
			raise Error, 'closed gzip stream' if closed?
			@closed = true
		end

		def orig_name
			raise Error, 'closed gzip stream' if closed?
			@orig_name
		end

		def comment
			raise Error, 'closed gzip stream' if closed?
			@comment
		end
	end

	# TODO: this class cheats a bit and just deflates everything at close time.
	# should be compressing in an incremental fashion...
	class GzipWriter < GzipFile
		def initialize io
			super()
			@io = io
			@header = false
			@mtime = Time.now # whats zlib's default in this regard...?
			@data = ''
		end

		def mtime
			raise GzipFile::Error, 'closed gzip stream' if closed?
			@mtime
		end

		def mtime= time
			raise GzipFile::Error, 'header is already written' if @header
			time = Time.at(time) if Numeric === time
			@mtime = time
		end

		def comment= comment
			raise GzipFile::Error, 'header is already written' if @header
			@comment = comment
		end

		def orig_name= orig_name
			raise GzipFile::Error, 'header is already written' if @header
			@orig_name = orig_name
		end

		def flush
			# NO OP currently
		end

		def close
			raise GzipFile::Error, 'closed gzip stream' if closed?
			# ensure header was written
			write ''
			# just compress everything now...
			data = Deflate.deflate(@data)
			data.slice! 0..1   # remove header
			data.slice! -4..-1 # remove adler
			@io.write data
			@io.write [Checksum.crc32(@data)].pack('V')
			@io.write [@data.length].pack('V')
			super
		end

		def write data
			unless @header
				@header = true
				header = ''
				# gzip header
				header << 0x1f
				header << 0x8b
				# deflate compression method
				header << 0x08
				# flags
				@ftext    = 0
				@fhcrc    = 0 # for now. maybe add later
				@fextra   = 0
				@fname    = 0 # TODO: handle @orig_name
				@fcomment = 0 # TODO: handle @comment
				header << [[@ftext, @fhcrc, @fextra, @fname, @fcomment].join].pack('b*')
				# mtime
				header << [@mtime.to_i].pack('V')
				# whats xfl?
				header << 0x00
				# OS. FIXME always unix here. maybe base on RUBY_PLATFORM?...
				header << 0x03
				@io.write header
			end
			@data << data
		end

		# no idea what wrap is supposed to do
		class << self
			def open io
				if String === io
					# not sure of the semantics in this case.
					# is it supposed to close the opened
					# io object on GzipFile#close??
					raise NotImplementedError unless block_given?
					File.open(io, 'wb') do |f|
						open(f) { |gz| yield gz }
					end
				else
					gz = new io
					if block_given?
						begin;  yield gz
						ensure; gz.close unless gz.closed?
						end
					else
						gz
					end
				end
			end

			alias wrap open
		end
	end

	class GzipReader < GzipFile
		OSES = [
			'FAT filesystem',
			'Amiga',
			'VMS (or OpenVMS)',
			'Unix',
			'VM/CMS',
			'Atari TOS',
			'HPFS fileystem (OS/2, NT)',
			'Macintosh',
			'Z-System',
			'CP/M',
			'TOPS-20',
			'NTFS filesystem (NT)',
			'QDOS',
			'Acorn RISCOS',
			'unknown'
		]

		def initialize io
			is_bit_set = proc do |val, bit|
				!!(val & (2 ** bit) == (2 ** bit))
			end

			super()
			@io = io
			io.read.each_byte {|b| @input_buffer << b}
			if @input_buffer[@in_pos+=1] != 0x1f || @input_buffer[@in_pos+=1] != 0x8b then raise Zlib::GzipFile::Error.new("not in gzip format") end
			if @input_buffer[@in_pos+=1] != 0x08 then raise Zlib::GzipFile::Error.new("unknown compression method") end
			flg = @input_buffer[@in_pos+=1]
			@ftext    = is_bit_set[flg, 0]
			@fhcrc    = is_bit_set[flg, 1]
			@fextra   = is_bit_set[flg, 2]
			@fname    = is_bit_set[flg, 3]
			@fcomment = is_bit_set[flg, 4]
			@mtime = Time.at(@input_buffer[@in_pos+=1] | (@input_buffer[@in_pos+=1] << 8) | (@input_buffer[@in_pos+=1] << 16) | (@input_buffer[@in_pos+=1] << 24))
			@xfl = @input_buffer[@in_pos+=1]
			@os = OSES[@input_buffer[@in_pos+=1]]
			if @fextra then
				@xlen = (@input_buffer[@in_pos+=1] | (@input_buffer[@in_pos+=1] << 8))
				@xtra_field = []
				@xlen.times {@xtra_field << @input_buffer[@in_pos+=1]}
			end
			if @fname then
				@orig_name = ""
				until @orig_name["\0"].nil? == false
					@orig_name.concat(@input_buffer[@in_pos+=1])
				end
				@orig_name.chop!
			end
			if @fcomment then
				@comment = ""
				until @comment["\0"].nil? == false
					@comment.concat(@input_buffer[@in_pos+=1])
				end
				@comment.chop!
			end
			if @fhcrc then
				@header_crc = @input_buffer[@in_pos+=1] | (@input_buffer[@in_pos+=1] << 8)
			end
			@contents = ""
			until @in_pos == @input_buffer.length-1
				@contents.concat(@input_buffer[@in_pos+=1])
			end

			#we do raw deflate, no headers
			@zstream = Zlib::Inflate.new -MAX_WBITS
			@inflated = StringIO.new(@zstream.inflate(@contents))
		end

		def read length=nil
			@inflated.read length
		end

		def eof?
			@inflated.eof?
		end

		def pos
			@inflated.pos
		end

		def rewind
			@inflated.rewind
			@io.seek 0, IO::SEEK_SET
		end

		class << self
			def open filename
				io = File.open filename
				gz = self.new io
				if block_given? then yield gz else gz end
			end
		end
	end
end

