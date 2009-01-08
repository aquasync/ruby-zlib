module Zlib
	class GzipFile
		def initialize
			@input_buffer = []
			@output_buffer = []
			@out_pos = -1
			@in_pos = -1
		end
	
		def close
		end
	
		class Error < Exception
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
				@original_name = ""
				until @original_name["\0"].nil? == false
					@original_name.concat(@input_buffer[@in_pos+=1])
				end
				@original_name.chop!
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

