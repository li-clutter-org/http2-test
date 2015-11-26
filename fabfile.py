"""

We need two special hostnames to connect to: 

StationA

and 

StationB

"""


from __future__ import with_statement, print_function
from fabric.api import local, settings, abort, run, sudo, cd, hosts, env, execute
from fabric.contrib.console import confirm
from fabric.operations import put, get
from fabric.contrib.project import rsync_project

import re
import subprocess as sp 
import os.path 
from   StringIO import StringIO


current_dir = os.path.dirname(os.path.realpath(__file__))


VNC_LICENSE = [
    "xxxxx-xxxxx-xxxxx-xxxxx-xxxxx"
]


# TODO: Put a proper deployment mechanism here.
env.key_filename = '/home/alcides/.ssh/zunzun_ec2_keypair_0.pem'

StationA_H = 'ubuntu@54.149.116.230'
StationB_H = 'ubuntu@54.149.116.233'
Beefy_H    = 'ubuntu@52.11.217.155'
# TODO: Make the IP number below deployment-specific...
Beefy_InternalIP = '192.168.112.131'
StationA_InternalIP = '192.168.112.129'
StationB_InternalIP = '192.168.112.130'
HomeDir_Name = "ubuntu"


@hosts(StationA_H)
def StationA():
    """
    Copies code to StationA 
    """
    rsync_project(
        local_dir = "scripts/StationA",
        remote_dir = ("/home/{HomeDir_Name}/".format(HomeDir_Name=HomeDir_Name))
        )
    run("ln -sf /home/{HomeDir_Name}/StationA/onstartup.py /home/{HomeDir_Name}/onstartup.py".format(HomeDir_Name=HomeDir_Name))


@hosts(StationB_H)
def setup_dns_masq():
    sudo("apt-get install -y dnsmasq")
    put(
        local_path  = StringIO("addn-hosts=/home/{HomeDir_Name}/dnsmasq_more.conf\n".format(HomeDir_Name=HomeDir_Name)),
        remote_path = "/etc/dnsmasq.conf",
        use_sudo=True)



@hosts(StationB_H)
def StationB():
    """
    Copies both the chrome plugin and the DNSMasq watcher 
    """
    rsync_project(
        local_dir = "scripts/StationB",
        remote_dir = ("/home/{HomeDir_Name}/".format(HomeDir_Name=HomeDir_Name))
        )
    rsync_project(
        local_dir = "scripts/StationA/chrome_captures_hars",
        remote_dir = (("/home/{HomeDir_Name}/StationB/".format(HomeDir_Name=HomeDir_Name)).format(HomeDir_Name=HomeDir_Name))
        )
    run("ln -sf /home/{HomeDir_Name}/StationB/onstartup.py /home/{HomeDir_Name}/onstartup.py".format(HomeDir_Name=HomeDir_Name))


@hosts(StationB_H)
def install_updatednsmasq_service():
    with settings(warn_only=True):
        sudo("service updatednsmasq stop")
    put(
        local_path = "scripts/StationB/configure_dnsmasq.py",
        remote_path = "/home/{HomeDir_Name}/StationB/configure_dnsmasq.py".format(HomeDir_Name=HomeDir_Name) ,
        use_sudo = True
        )
    put(
        local_path = StringIO("""
description "Update dnsmasq"

start on runlevel [2345]
stop on runlevel [!2345]

umask 022

console log

env PATH=/opt/openssl-1.0.2/bin/:/usr/bin:/usr/local/bin:/usr/sbin:/bin 
export PATH
env LD_LIBRARY_PATH=/opt/openssl-1.0.2/lib
export LD_LIBRARY_PATH
env USER={HomeDir_Name}
export USER

script 
    exec /usr/bin/python /home/{HomeDir_Name}/StationB/configure_dnsmasq.py
end script

""".format(HomeDir_Name=HomeDir_Name)),
        remote_path = "/etc/init/updatednsmasq.conf",
        use_sudo=True )
    sudo("service updatednsmasq start")


@hosts(Beefy_H)
def Beefy():
    sudo("apt-get update")
    sudo("apt-get -y install libgmp-dev")


@hosts(Beefy_H)
def BeefyRehMimic():
    with settings(warn_only=True):
        sudo("service mimic stop")
    put(
        local_path  = "dist/build/reh-mimic/reh-mimic",
        remote_path = "/home/{HomeDir_Name}/reh-mimic".format(HomeDir_Name=HomeDir_Name)
        )
    run("chmod ugo+x /home/{HomeDir_Name}/reh-mimic".format(HomeDir_Name=HomeDir_Name))
    sudo("rm /home/{HomeDir_Name}/mimic -rf".format(HomeDir_Name=HomeDir_Name) )
    rsync_project(
        local_dir  = "mimic",
        remote_dir = "/home/{HomeDir_Name}/".format(HomeDir_Name=HomeDir_Name),
        )
    put(
        local_path  = "scripts/mimic.conf",
        remote_path = "/etc/init/mimic.conf",
        use_sudo    = True
        )
    sudo("touch /root/.rnd")
    sudo("service mimic start")



@hosts(Beefy_H, StationA_H, StationB_H )
def configure_logging():
    if env.host_string == StationA_H:
        put(
            local_path = StringIO("""$template Logentries,"199fb2e1-8227-4f73-9150-70a34a5d5e0c %HOSTNAME% %syslogtag%%msg%\\n"
*.* @@api.logentries.com:10000;Logentries"""),
            remote_path = "/etc/rsyslog.d/70-logentries.conf",
            use_sudo = True )
    elif env.host_string == StationB_H:
        put(
            local_path = StringIO("""$template Logentries,"3d2fd756-407a-4764-b130-1dd6f22a1b62 %HOSTNAME% %syslogtag%%msg%\\n"
*.* @@api.logentries.com:10000;Logentries"""),
            remote_path = "/etc/rsyslog.d/70-logentries.conf",
            use_sudo = True )
    else:
        put(
            local_path = StringIO("""$template Logentries,"7551d4e0-fa76-466f-8547-8c9a347a9363 %HOSTNAME% %syslogtag%%msg%\\n"
*.* @@api.logentries.com:10000;Logentries"""),
            remote_path = "/etc/rsyslog.d/70-logentries.conf",
            use_sudo = True )
        
    sudo("service rsyslog restart")
    # Check logging works...
    sudo("logger -t test Hello there Logentries")


@hosts (StationA_H, StationB_H)
def deploy_specific():
    if env.host_string == StationA_H:
        print("StationA deploy")
        StationA()
    elif env.host_string == StationB_H:
        print("StationB deploy")
        StationB()
    else: 
        print("Beefy station deploy")
        Beefy()


@hosts(StationA_H, StationB_H)
def apt_stations():
    sudo("apt-get update")
    sudo("apt-get install -y xutils xbase-clients xfonts-base xfonts-75dpi xfonts-100dpi")
    sudo("apt-get install -y python-pip")
    sudo("apt-get install -y xdotool")
    sudo("apt-get install -y xfwm4") 


@hosts(StationA_H, StationB_H)
def pythonlibs():
    sudo("pip install python-daemon>=2.0")
    sudo("pip install raven")


@hosts(Beefy_H, StationA_H, StationB_H)
def ssl():
    """
    Copies Openssl and curl to the target hosts...
    """
    sudo("mkdir -p /opt/openssl-1.0.2/")
    sudo(("chown {HomeDir_Name} /opt/openssl-1.0.2/".format(HomeDir_Name=HomeDir_Name)))
    rsync_project(
        local_dir = "/opt/openssl-1.0.2",
        remote_dir = "/opt/",
        extra_opts="-avz"
        )
    put(
        local_path = "scripts/ca-certificates.crt",
        remote_path = "/etc/ssl/certs/ca-certificates.crt",
        use_sudo = True
        )


@hosts(Beefy_H, StationA_H, StationB_H)
def ca():
    """
    Copies the ca certificate to the home...
    """
    put(
        local_path = "mimic-here/config/ca/cacert.pem",
        remote_path = ("/home/{HomeDir_Name}/cacert.pem".format(HomeDir_Name=HomeDir_Name)),
        use_sudo = True
        )


@hosts(StationA_H, StationB_H)
def install_vnc():
    """
    
    """
    # run("curl -L -o VNC.tar.gz https://www.realvnc.com/download/binary/1720/")
    # run("tar xvf VNC-5.2.3-Linux-x64-ANY.tar.gz")


    put(
        local_path = "scripts/VNC-5.2.3-Linux-x64-ANY.tar.gz",
        remote_path = ("/home/{HomeDir_Name}/VNC-5.2.3-Linux-x64-ANY.tar.gz".format(HomeDir_Name=HomeDir_Name)),
        use_sudo = True
        )
    run(("tar -xzf /home/{HomeDir_Name}/VNC-5.2.3-Linux-x64-ANY.tar.gz".format(HomeDir_Name=HomeDir_Name)))
    # Get a handier name.... 
    run("rm -rf vnc")
    run(("mv /home/{HomeDir_Name}/VNC-5.2.3-Linux-x64 /home/{HomeDir_Name}/vnc".format(HomeDir_Name=HomeDir_Name)))
    sudo(("/home/{HomeDir_Name}/vnc/vnclicense -add {VncLicense}".format(
        HomeDir_Name= HomeDir_Name,
        VncLicense = VNC_LICENSE[0]
    )))
    # Will demand some for of interactive input...
    run(("mkdir -p /home/{HomeDir_Name}/.vnc/".format(HomeDir_Name=HomeDir_Name)))
    run(("mkdir -p /home/{HomeDir_Name}/.vnc/config.d/".format(HomeDir_Name=HomeDir_Name)))
    sudo(("/home/{HomeDir_Name}/vnc/vncpasswd /home/{HomeDir_Name}/.vnc/config.d/Xvnc".format(HomeDir_Name=HomeDir_Name)))
    vnc_fix_permissions()

@hosts(StationA_H, StationB_H)
def vnc_fix_permissions():
    sudo(("chown {HomeDir_Name} /home/{HomeDir_Name}/.vnc -R").format(HomeDir_Name=HomeDir_Name))

@hosts(StationA_H, StationB_H)
def install_vnc_xstartup():
    run(("mkdir -p /home/{HomeDir_Name}/.vnc/".format(HomeDir_Name=HomeDir_Name)))
    run(("mkdir -p /home/{HomeDir_Name}/.vnc/config.d/".format(HomeDir_Name=HomeDir_Name)))
    put(
        local_path = "scripts/vnc-xstartup",
        remote_path = ("/home/{HomeDir_Name}/.vnc/xstartup".format(HomeDir_Name=HomeDir_Name))
        )
    run("chmod ugo+x /home/{HomeDir_Name}/.vnc/xstartup".format(HomeDir_Name=HomeDir_Name))
    put(
        local_path = "scripts/xvncfontpath",
        remote_path = ("/home/{HomeDir_Name}/.vnc/config.d/xvncfontpath".format(HomeDir_Name=HomeDir_Name))
        )


@hosts(StationA_H, StationB_H)
def setup_google_chrome():
    put(
        local_path = "scripts/google-chrome-stable_current_amd64.deb",
        remote_path = ("/home/{HomeDir_Name}/google-chrome-stable_current_amd64.deb".format(HomeDir_Name=HomeDir_Name)),
        use_sudo = True
        )
    really_setup_google_chrome()


@hosts(Beefy_H, StationA_H, StationB_H)
def ensure_local_hosts():
    # Get the contents of /etc/hosts
    local_file = StringIO()
    get(
        local_path = local_file,
        remote_path = "/etc/hosts",
        use_sudo = True 
        )
    hosts_file = local_file.getvalue()
    snippet =  """# DO NOT EDIT BELOW BY HAND
{Beefy_InternalIP} instr.httpdos.com
192.168.112.129 ip-192-168-112-129
192.168.112.130 ip-192-168-112-130
192.168.112.131 ip-192-168-112-131
# END DO NOT EDIT BELOW""".format(
    StationA_InternalIP = StationA_InternalIP,
    Beefy_InternalIP    = Beefy_InternalIP
    )
    mo = re.search(r"# DO NOT EDIT BELOW BY HAND\n(.*?)\n# END DO NOT EDIT BELOW", hosts_file, re.MULTILINE)
    if mo:
        part_before = hosts_file[:mo.start(0)]
        part_after = hosts_file[mo.end(0):]
        hosts_file = part_before + snippet + part_after
    else:
        hosts_file += "\n" + snippet

    put(
        local_path = StringIO(hosts_file),
        remote_path = "/etc/hosts",
        use_sudo = True
        )


@hosts(StationA_H, StationB_H)
def really_setup_google_chrome():
    sudo("apt-get update")
    sudo(("apt-get -f install -y".format(HomeDir_Name=HomeDir_Name)))
    sudo("apt-get install -y --fix-missing xdg-utils")
    sudo(("dpkg -i --force-depends /home/{HomeDir_Name}/google-chrome-stable_current_amd64.deb".format(HomeDir_Name=HomeDir_Name)))
    sudo(("apt-get -f install -y".format(HomeDir_Name=HomeDir_Name)))


@hosts(StationA_H, StationB_H)
def setup_vnc_service():
    put(
        local_path = "scripts/vncserv-{HomeDir_Name}.conf".format(HomeDir_Name=HomeDir_Name),
        remote_path = "/etc/init/vncserv.conf",
        use_sudo = True
        )
    put(
        local_path = "scripts/undaemon.py",
        remote_path = "/home/{HomeDir_Name}/undaemon.py".format(HomeDir_Name=HomeDir_Name)
        )
    run("chmod ugo+x /home/{HomeDir_Name}/undaemon.py".format(HomeDir_Name=HomeDir_Name))
    with settings(warn_only=True):
        sudo(
            "service vncserv start"
            )


@hosts(StationA_H, StationB_H)
def  disable_lightdm():
    contents = StringIO("manual")
    put(
        local_path = contents, 
        remote_path = "/etc/init/lightdm.override",
        use_sudo=True
        )


@hosts(StationA_H, StationB_H)
def touch_xauthority():
    run("touch $HOME/.Xauthority")


@hosts(StationA_H, StationB_H)
def deploy():
    execute(apt_stations)
    execute(setup_dns_masq)
    execute(setup_google_chrome)
    execute(deploy_specific)
    execute(touch_xauthority)
    execute(disable_lightdm)
    execute(StationA)
    execute(StationB)
    execute(Beefy)
    execute(ca)
    execute(ssl)
    execute(install_vnc)
    execute(install_vnc_xstartup)
    execute(ensure_local_hosts)
    execute(setup_vnc_service)
    execute(pythonlibs)
    execute(BeefyRehMimic)
    execute(install_updatednsmasq_service)    
