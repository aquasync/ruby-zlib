require File.dirname(__FILE__) + '/../spec_helper'

describe 'Zlib::Inflate#set_dictionary' do
  it 'sets the inflate dictionary' do
    deflated = "x\273\024\341\003\313KLJNIMK\317\310\314\002\000\025\206\003\370"

    i = Zlib::Inflate.new

    begin
      i << deflated
      raise Exception.new('Zlib::NeedDict not raised')
    rescue Zlib::NeedDict
      i.set_dictionary 'aaaaaaaaaa'
    end

    i.finish.should == 'abcdefghij'
  end
end

