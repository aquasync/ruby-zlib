require 'enumerator'
require 'stringio'

# if the c implemention of Zlib is already defined, rename it to CZlib
# and take over. this happens for me running the tests as rspec is
# installed as a gem.
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

require 'zlib/version'
require 'zlib/common'
require 'zlib/checksum'
require 'zlib/inflate'
require 'zlib/deflate'
require 'zlib/gzip'
