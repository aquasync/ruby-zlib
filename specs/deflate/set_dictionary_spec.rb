require File.dirname(__FILE__) + '/../spec_helper'

describe 'Zlib::Deflate#set_dictionary' do
  it 'sets the dictionary' do
  	['aaaaaaaaaa', 'abcdefghij'].each do |dict|
  		d = CZlib::Deflate.new
			d.set_dictionary dict
			d << 'abcdefghij'
	    compr = d.finish
	    # my code gets compr:
	    #
	    #   "x\273\024\341\003\313KLJNIMK\317\310\314\002\000\025\206\003\370"
	    #   "x\273\025\206\003\370C\260\000\025\206\003\370"
	    #
	    # zlib churns out:
	    #
			#   "x\273\024\341\003\313KLJNIMK\317\310\314\002\000\025\206\003\370"
			#   "x\273\025\206\003\370K\204\263\000\025\206\003\370"
			#
	    # now uncompress. CZlib has a quirk in how you use
	    # Inflate#set_dictionary in that you have to have
	    # an exception first
	    begin
      	inf = CZlib::Inflate.new
      	inf.inflate compr
      rescue CZlib::NeedDict
      end
      inf.set_dictionary dict
      inf.inflate(compr).should == 'abcdefghij'
    end
  end
end
