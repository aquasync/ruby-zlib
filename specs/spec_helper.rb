# for now we require the c version additionally for some test assertions.
# this will be removed when i fix the specs.
require 'zlib.so' unless defined?(CZlib)

path = File.expand_path(File.dirname(__FILE__) + '/../lib')
$:.unshift path unless $:.include?(path)

require 'zlib.rb'
