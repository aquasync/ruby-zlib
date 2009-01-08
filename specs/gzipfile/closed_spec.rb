#require File.dirname(__FILE__) + '/../../spec_helper'
require '../lib/zliby'
require 'stringio'


describe 'Zlib::GzipFile#closed?' do
  it 'returns the closed status' do
    io = StringIO.new
    Zlib::GzipWriter.wrap io do |gzio|
      gzio.closed?.should == false

      gzio.close

      gzio.closed?.should == true
    end
  end
end

