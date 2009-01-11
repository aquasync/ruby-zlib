require 'rake/rdoctask'
require 'rake/packagetask'
require 'rake/gempackagetask'
require 'spec/rake/spectask'
require 'spec/rake/verify_rcov'

require 'rbconfig'
require 'fileutils'

# for now just get version info. later on i'll probably make it use
# the ruby zlib for the packaging tasks
#load './lib/zlib/version.rb'
$:.unshift 'lib'
require 'zlib/version'

PKG_NAME = 'ruby-zlib'
PKG_VERSION = Zlib::RUBY_ZLIB_VERSION

task :default => :spec

desc 'Run all specs'
Spec::Rake::SpecTask.new :spec do |t|
	t.spec_opts = ['--format specdoc --colour']
	t.pattern = 'specs/**/*_spec.rb'
	# otherwise rake prints out the annoyingly large command line
	t.fail_on_error = false
end

desc 'Run all specs and generate html spec document'
namespace :spec do
	Spec::Rake::SpecTask.new :html do |t|
		t.spec_opts = ['--format html:spec.html']
		t.pattern = 'specs/**/*_spec.rb'
		t.fail_on_error = false
	end
end

desc 'Run all specs and generate coverage'
Spec::Rake::SpecTask.new :rcov do |t|
	t.rcov = true
	t.pattern = 'specs/**/*_spec.rb'
	t.rcov_opts = ['--exclude', 'specs']
	t.rcov_opts << '--xrefs'
	t.rcov_opts << '--text-report'
	t.fail_on_error = false
end

namespace :rcov do
	RCov::VerifyTask.new :verify => :rcov do |t|
		t.threshold = 80.0
		t.index_html = 'coverage/index.html'
	end
end

Rake::RDocTask.new do |t|
	t.rdoc_dir = 'doc'
	t.rdoc_files.include 'lib/**/*.rb'
	t.rdoc_files.include 'README'
	t.title    = "#{PKG_NAME} documentation"
	t.options += %w[--line-numbers --inline-source --tab-width 2]
	t.main	   = 'README'
end

spec = Gem::Specification.new do |s|
	s.name = PKG_NAME
	s.version = PKG_VERSION
	s.summary = %q{Pure ruby zlib implementation.}
	s.description = %q{A complete replacement for the standard c zlib extension written in ruby.}
	s.authors = ['Charles Lowe']
	s.email = %q{aquasync@gmail.com}
	s.homepage = %q{http://github.com/aquasync/ruby-zlib}
	s.rubyforge_project = %q{ruby-zlib}

	s.files  = ['README', 'Rakefile']
	s.files += FileList['lib/**/*.rb']
	s.files += FileList['specs/**/*.rb']

	s.has_rdoc = true
	s.extra_rdoc_files = ['README']
	s.rdoc_options += [
		'--main', 'README',
		'--title', "#{PKG_NAME} documentation",
		'--tab-width', '2'
	]
end

Rake::GemPackageTask.new(spec) do |t|
	t.gem_spec = spec
	t.need_tar = false
	t.need_zip = false
	t.package_dir = 'build'
end

