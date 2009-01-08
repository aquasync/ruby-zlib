require File.dirname(__FILE__) + '/../spec_helper'

describe 'Zlib::Inflate#<<' do
  it 'appends data to the input stream' do
    zs = Zlib::Inflate.new
    zs << "x\234K\313\317\a\000\002\202\001E"

    zs.finish.should == 'foo'
  end
end

