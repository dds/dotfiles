require 'rake/clean'

here = File.dirname(__FILE__)
build = File.join(here, 'build')
CLEAN.include('build/*')

# These will each be installed into the target
symlinks = FileList[
  '.bashrc',
  '.bash_profile',
  '.profile',
  '.emacs.d',
  '.screenrc',
  '.spacemacs.d',
  '.gitconfig'
]

# These generated files will be copied into the target
generated = FileList[
  'build/.msmtprc',
  'build/.mbsyncrc'
]

task :install, [:prefix] => [:build] do |t, args|
  args.with_defaults(:prefix => "target")

  mkdir_p args.prefix

  symlinks.each do |f|
    from = File.join(here, f)
    to = File.join(args.prefix, f)
    rm_f(to)
    ln_sf(from, to)
  end

  generated.each do |f|
    from = File.join(here, f)
    to = File.join(args.prefix, File.basename(f))
    install(from, to)
  end
end

task :build do |t|
  mkdir_p build

  mbsyncrc = File.join(build, '.mbsyncrc')
  genmbsyncrc = File.join(here, 'bin/genmbsyncrc')
  sh %{ #{genmbsyncrc} >| #{mbsyncrc} }

  msmtprc = File.join(build, '.msmtprc')
  genmsmtprc = File.join(here, 'bin/genmsmtprc')
  sh %{ #{genmsmtprc} >| #{msmtprc} }
end

