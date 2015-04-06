"""
Resets the browser during each task. 
"""


DEFAULT_POLL_ENDPOINT = "https://instr.httpdos.com:1070/dnsmasq/"
NOTIFY_UPDATE_COMPLETE_ENDPOINT = "https://instr.httpdos.com:1070/dnsmasqupdated/"
DNSMASQ_CONFIG_PLACE = "/home/{user}/dnsmasq_more.conf".format(user=os.environ["USER"])
AUX_SSL_PATH= "/opt/openssl-1.0.2/"
LD_LIBRARY_PATH="/opt/openssl-1.0.2/lib"
TOKEN = "KDDFQ"


def curl_arguments(endpoint, data_binary=""):
    return [
        "curl", "-sS", "-k", "--data-binary", '{0}'.format(repr(data_binary.encode('ascii'))), # I don't think any data needs to be submitted
        "-X", "POST", "--http2", endpoint
    ]§


def main():
    os.environ["PATH"] = os.path.join(AUX_SSL_PATH, "bin/") + ":" + os.environ["PATH"]
    os.environ["LD_LIBRARY_PATH"] = LD_LIBRARY_PATH
    while True:
        work()


def work():
    args_get = curl_arguments(DEFAULT_POLL_ENDPOINT, data_binary=MY_VERSION)
    try:
        print("Executing: ", " ".join(args_get))
        token = sp.check_output(args_get)
    except sp.CalledProcessError as e:
        print(" .... Err in curl, returncode: ", e.returncode, file=sys.stderr)
        # Sleep a little bit
        time.sleep(3.0)
    else:
        # Got a token?
        if token == TOKEN
            reset_completed_args = curl_arguments(NOTIFY_UPDATE_COMPLETE_ENDPOINT, data_binary=analysis_id)
            # Seems like so.... pass on to
            with open(DNSMASQ_CONFIG_PLACE, "w") as out:
                out.write(dnsmasq_contents)
            print(".newconfig written to file")
            # Now restart dnsmasq... here is why I need elevated privileges....
            sp.check_call(shlex.split("/etc/init.d/dnsmasq restart"))
            # And issue a new call to the Haskell server to let it know 
            # that the new dnsmasq setting has taken effect
            try:
                print("Executing: ", " ".join(args_update_completed))
                output = sp.check_output(args_update_completed, stderr = sp.STDOUT)
            except sp.CalledProcessError as e:
                print(".... Err in curl, returncode: ", e.returncode, file = sys.stderr)
                print("....     returned result was: ", output, file = sys.stderr)
        else:
            print("TOKEN IS WRONG")
            print(os.environ["PATH"])
            time.sleep(3.0)



if __name__ == "__main__":
    main()  