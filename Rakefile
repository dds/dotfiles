require 'rake/clean'

here = File.dirname(__FILE__)
build = File.join(here, 'build')
tools = File.join(here, 'tools')
bin = File.join(here, 'bin')
is_linux = RbConfig::CONFIG['host_os'] =~ /linux/i
is_mac = RbConfig::CONFIG['host_os'] =~ /darwin/i

# These directories will be created in the target
dirs = FileList[
  '.ssh',
  '.gnupg',
  '.local/bin',
]

# These will each be installed into the target
symlinks = FileList[
  '.bash',
  '.bashrc',
  '.bash_profile',
  '.profile',
  '.doom.d',
  '.screenrc',
  '.spacemacs.d',
  '.gitconfig',
  '.gitconfig-fbinfra',
  '.dircolors',
  '.vimrc',
  '.zshrc',
  '.gnupg/gpg-agent.conf',
  '.gnupg/gpg.conf',
]

# Linux-only symlinks (X11, desktop entries)
linux_symlinks = FileList[
  '.Xresources',
  '.local/share/applications/org-protocol.desktop',
]

# Systemd units to enable
systemd_units = [
  'mbsync.timer',
  'keybase.service',
  'kbfs.service',
  'syncthing.service',
]

# These generated files will be copied into the target
generated = FileList[
  'build/.msmtprc',
  'build/.mbsyncrc'
]
CLEAN.include(generated)


task :build do |t|
  accounts = File.join(here, 'private/gmailaccounts')
  unless File.exist?(accounts)
    abort "#{accounts} not found. Create it with one Gmail address per line."
  end

  mkdir_p build

  mbsyncrc = File.join(build, '.mbsyncrc')
  genmbsyncrc = File.join(tools, 'genmbsyncrc')
  sh %{ #{genmbsyncrc} < #{accounts} >| #{mbsyncrc} }

  msmtprc = File.join(build, '.msmtprc')
  genmsmtprc = File.join(tools, 'genmsmtprc')
  sh %{ #{genmsmtprc} < #{accounts} >| #{msmtprc} }
end


task :install, [:prefix] do |t, args|
  args.with_defaults(:prefix => "target")

  mkdir_p args.prefix

  dirs.each do |d|
    dest = File.join(args.prefix, d)
    mkdir_p dest
  end

  generated.each do |f|
    from = File.join(here, f)
    to = File.join(args.prefix, File.basename(f))
    File.exist?(from) and install(from, to)
  end

  symlinks.each do |f|
    from = File.join(here, f)
    to = File.join(args.prefix, f)
    rm_f(to)
    File.exist?(from) and ln_sf(from, to)
  end

  if is_linux
    mkdir_p File.join(args.prefix, '.local/share/applications')

    linux_symlinks.each do |f|
      from = File.join(here, f)
      to = File.join(args.prefix, f)
      rm_f(to)
      File.exist?(from) and ln_sf(from, to)
    end

    systemd_dir = '.config/systemd/user'
    prefix_systemd_dir = File.join(args.prefix, systemd_dir)
    mkdir_p prefix_systemd_dir
    local_systemd_dir = File.join(here, systemd_dir)
    Dir.glob(File.join(local_systemd_dir, '*')).each do |f|
      to = File.join(prefix_systemd_dir, File.basename(f))
      ln_sf(f, to)
    end

    autostart_dir = '.config/autostart'
    prefix_autostart_dir = File.join(args.prefix, autostart_dir)
    mkdir_p prefix_autostart_dir
    local_autostart_dir = File.join(here, autostart_dir)
    Dir.glob(File.join(local_autostart_dir, '*')).each do |f|
      to = File.join(prefix_autostart_dir, File.basename(f))
      ln_sf(f, to)
    end

    systemd_units.each do |f|
      sh %{ systemctl --user enable #{f} || true }
    end

    sh %{ update-desktop-database #{args.prefix}/.local/share/applications || true }
  end

  Dir.glob(File.join(bin, '*')).each do |f|
    to = File.join(args.prefix, '.local/bin')
    ln_sf(f, to)
  end

  # Detect screen version and symlink the appropriate colors config
  screen_v5 = false
  begin
    ver = `screen --version 2>/dev/null`
    if ver =~ /Screen version (\d+)/
      screen_v5 = $1.to_i >= 5
    end
  rescue
  end
  colors_src = screen_v5 ? '.screenrc.colors.v5' : '.screenrc.colors.legacy'
  colors_from = File.join(here, colors_src)
  colors_to = File.join(args.prefix, '.screenrc.colors')
  rm_f(colors_to)
  ln_sf(colors_from, colors_to)
end
