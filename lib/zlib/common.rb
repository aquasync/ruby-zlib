
module Zlib
	# For compatibility with Ruby-core Zlib
	ZLIB_VERSION = '1.2.3'
	VERSION = '0.6.0'

	MAX_WBITS = 15
	DEF_MEM_LEVEL = 6 # ? FIXME

	# compression levels
	NO_COMPRESSION       =  0
	BEST_SPEED           =  1
	BEST_COMPRESSION     =  9
	DEFAULT_COMPRESSION  = -1

	# strategies
	FILTERED           = 1
	HUFFMAN_ONLY       = 2
	RLE                = 3
	FIXED              = 4
	DEFAULT_STRATEGY   = 0

	# flush values
	NO_FLUSH      = 0
	PARTIAL_FLUSH = 1 # will be removed, use Z_SYNC_FLUSH instead
	SYNC_FLUSH    = 2
	FULL_FLUSH    = 3
	FINISH        = 4
	BLOCK         = 5

	# misc. go through these properly later, but i think these are ones
	# that CZlib doesn't expose
	PRESET_DICT        = 0x20 # preset dictionary flag in zlib header
	DEFLATED           = 8

	#Generic Error
	class Error < StandardError
	end

	#Dictionary Needed
	class NeedDict < Error
	end

	#Invalid Data
	class DataError < Error
	end

	class ZStream
		class BitReader # :nodoc:
			attr_reader :io

			def initialize io
				@io = io
				@buf = 0
				@have = 0
			end

			def read want
				val = @buf
				while @have < want
					raise EOFError unless c = @io.getc
					val |= c << @have
					@have += 8
				end

				@buf = val >> want
				@have -= want
				val & ((1 << want) - 1)
			end
		end

		class BitWriter # :nodoc:
			attr_reader :io

			def initialize io
				@io = io
				@buf = 0
				@have = 0
			end

			def write val, count=1
		    @buf |= val << @have
		    @have += count
		    while @have >= 8
		    	@io  << (@buf & 0xff).chr
					@buf >>= 8
					@have -= 8
				end
			end
		end
	end
end


