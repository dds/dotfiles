desc "Install dotfiles"
task :install, [:prefix] do |t, args| 
  args.with_defaults(:prefix => "target")
  mkdir_p args.prefix
  symlinks = FileList[
    '.bashrc',
    '.bash_profile',
    '.profile',
    '.emacs.d',
    '.spacemacs.d',
  ]
  symlinks.each do |target|
    from = File.join(pwd, target)
    to = File.join(args.prefix, target)
    rm(to)
    ln_sf(from, to)
  end
end
