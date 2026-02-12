# -*- mode: ruby -*-
Vagrant.configure("2") do |config|
  config.vm.box = "bento/ubuntu-24.04"
  config.ssh.forward_agent = true
  config.vm.provider "virtualbox" do |vb|
    vb.gui = true
    vb.memory = "2048"
  end

  config.vm.provision "shell",
                      path: "bootstrap/provision_ubuntu_amd64.sh",
                      env: {
                        "USER" => "vagrant",
                        "SRCDIR" => "/vagrant",
                      }
end
