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

# Systemd units to enable
systemd_units = [
  'mbsync.timer',
  'keybase.service',
  'kbfs.service',
  'syncthing.service',
]

task :install, [:prefix] do |t, args|
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

  systemd_dir = '.config/systemd/user'
  prefix_systemd_dir = File.join(args.prefix, systemd_dir)
  mkdir_p prefix_systemd_dir
  local_systemd_dir = File.join(here, systemd_dir)
  Dir.glob(File.join(local_systemd_dir, '*')).each do |f| 
    to = File.join(prefix_systemd_dir, File.basename(f))
    ln_sf(f, to)
  end
  systemd_units.each do |f|
    sh %{ systemctl --user enable #{f} }
  end

  autostart_dir = '.config/autostart'
  prefix_autostart_dir = File.join(args.prefix, autostart_dir)
  mkdir_p prefix_autostart_dir
  local_autostart_dir = File.join(here, autostart_dir)
  Dir.glob(File.join(local_autostart_dir, '*')).each do |f|
    to = File.join(prefix_autostart_dir, File.basename(f))
    ln_sf(f, to)
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

