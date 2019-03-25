require 'rake/clean'

desc "Install dotfiles"
task :install, [:prefix] => [:pythonsetup] do |t, args|
  here = File.dirname(__FILE__)
  args.with_defaults(:prefix => "target")
  mkdir_p args.prefix

  symlinks = FileList[
    '.bashrc',
    '.bash_profile',
    '.profile',
    '.emacs.d',
    '.spacemacs.d',
    '.gitconfig'
  ]
  symlinks.each do |f|
    from = File.join(here, f)
    to = File.join(args.prefix, f)
    rm_f(to)
    ln_sf(from, to)
  end

  mbsyncrc = File.join(args.prefix, '.mbsyncrc')
  genmbsyncrc = File.join(here, 'bin/genmbsyncrc')
  sh %{ #{genmbsyncrc} >| #{mbsyncrc} }
end

task :pythonsetup do
  sh %{ pip install -q -r requirements.txt }
end
