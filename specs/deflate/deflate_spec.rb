require File.dirname(__FILE__) + '/../spec_helper'

# these specs should be re-written to rely on working inflate functionality. unlike
# inflate, there are many possible valid outputs from compression.
# that way these specs can be tested on CZlib as well

describe 'Zlib::Deflate#deflate' do

  before :each do
    @deflator = Zlib::Deflate.new
  end

  it 'deflates some data' do
    data = "\000" * 10

    zipped = @deflator.deflate data, Zlib::FINISH
    @deflator.finish

    #zipped.should == "x\234c\200\003\000\000\n\000\001"
    CZlib::Inflate.inflate(zipped).should == data
  end

#  it 'deflates lots of data' do
#    data = "\000" * 32 * 1024
#
#    zipped = @deflator.deflate data, Zlib::FINISH
#    @deflator.finish
#
#    zipped.should == "x\234\355\301\001\001\000\000\000\200\220\376\257\356\b\n#{"\000" * 31}\030\200\000\000\001"
#  end

  it 'deflates data incrementally' do
		strings = ['some data ', 'some more data ', 'and even more data']

		zipped = ''
		strings.each { |s| zipped << @deflator.deflate(s, Zlib::NO_FLUSH) }
		zipped << @deflator.finish
#		zipped = @deflator.deflate(strings.join, Zlib::FINISH)

    CZlib::Inflate.inflate(zipped).should == strings.join
  end

end

describe 'Zlib::Deflate::deflate' do

  it 'deflates some data' do
    data = "\000" * 10

    zipped = Zlib::Deflate.deflate data

    #zipped.should == "x\234c\200\003\000\000\n\000\001"
    CZlib::Inflate.inflate(zipped).should == data
  end

#  it 'deflates lots of data' do
#    data = "\000" * 32 * 1024
#
#    zipped = Zlib::Deflate.deflate data
#
#    zipped.should == "x\234\355\301\001\001\000\000\000\200\220\376\257\356\b\n#{"\000" * 31}\030\200\000\000\001"
#  end

  it 'can specify an compression level' do
    data = 'blah blah blah blah blah'

    zipped = Zlib::Deflate.deflate data
    zipped.length.should < data.length
    CZlib::Inflate.inflate(zipped).should == data

    zipped = Zlib::Deflate.deflate data, Zlib::NO_COMPRESSION
    zipped.length.should > data.length
    zipped[7, data.length].should == data
    CZlib::Inflate.inflate(zipped).should == data
  end

end

