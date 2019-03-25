#!/bin/sh
USER=${USER:-vagrant}
TARGET=${TARGET:-$(getent passwd "$USER" | cut -d: -f6)}

### Set environment variables
export DEBIAN_FRONTEND=noninteractive

### Add external package sources
add-apt-repository --yes --no-update ppa:kelleyk/emacs
curl -sLS https://deb.nodesource.com/setup_10.x | sh -x

### Update packages
apt-get update

### Remove conflicting packages
apt-get remove -y --purge \
  emacs25-common

### Install extra packages
apt-get install -y --ignore-missing \
  emacs26 \
  gdm3 \
  gnome-session \
  gnome-terminal \
  gpg \
  gpg-agent \
  isync \
  msmtp \
  python3 \
  python3-pip \
  python3-jinja2 \
  nodejs \
  ruby \
  screen \
  syncthing \
  chrome-gnome-shell \
  chromium-browser \
  greybird-gtk-theme \
  x11-xkb-utils \
  workrave

if ! dpkg-query -s keybase >/dev/null; then
    curl -sLS https://prerelease.keybase.io/keybase_amd64.deb -o /tmp/keybase_amd64.deb
    dpkg -i /tmp/keybase_amd64.deb
fi

### Bring the whole system up to date
apt-get -u -y -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" dist-upgrade

### Compile tools from downloaded software
gem install rake

### Configure
systemctl restart gdm3

### Install dotfiles
sudo -u $USER rake -f/dotfiles/Rakefile install[$TARGET]
