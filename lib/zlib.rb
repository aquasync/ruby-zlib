require 'enumerator'
require 'stringio'

# for testing...
require 'zlib'

# for testing purposes this makes it clearer. Zlib is already defined when i'm
# running specs as rspec is installed as a gem.
if defined? Zlib
	CZlib = Zlib
	begin
		old_verbose = $VERBOSE
		$VERBOSE = nil
		Zlib = Module.new
	ensure
		$VERBOSE = old_verbose
	end
end

$:.unshift File.dirname(__FILE__)

require 'zlib/common'
require 'zlib/checksum'
require 'zlib/inflate'
require 'zlib/deflate'

if $0 == __FILE__
	p CZlib::Deflate.deflate('mydatamydata')
	# => "x\234\313\255LI,I\314\005\223\000 \332\005\001"
	# the "x\234" is header gunk...
	p [Zlib.adler32('mydatamydata')].pack('N')
	# => " \332\005\001"
	# that last 4 bytes is the adler checksum...
	# the rest is the actual compressed data
	# -- "\313\255LI,I\314\005\223\000"
	# actually manages 2 bytes off.
	(q = Zlib::Deflate.new).deflate 'mydatamydata', Zlib::FINISH
	data = q.output
	p :deflated => data
	p CZlib::Inflate.inflate(data)
end

#789c cbad 4c49 2c49 8490 0020 da05 01
# => "x\234\313\255LI,I\204\220\000 \332\005\001"


