#!/usr/bin/python

from __future__ import print_function

try:
    import redis 
except ImportError:
    redis = None
    
import os 
import sys
import findout_instance_ips
    
# Time in seconds
HOLD_CACHE_TIME = 3600


class SuperfluosCache(object):
    def __init__(self):
        pass 
    
    def __setitem__(self, key, value):
        pass 

    def get(self, key):
        return None


class RedisCache(object):
    def __init__(self, prefix="_st_login_"):
        assert(redis is not None)
        self._conn = redis.StrictRedis()
        self._prefix = prefix
        
    def _mk_redis_key(self, k):
        return self._prefix + k
    
    def __setitem__(self, key, value):
        k = self._mk_redis_key(key)
        self._conn.set( k, value)
        self._conn.expire( k, HOLD_CACHE_TIME )
    
    def get(self, key):
        k = self._mk_redis_key(key)
        value = self._conn.get( k )
        if value is not None:
            self._conn.persist( k )
            self._conn.expire(k, HOLD_CACHE_TIME)
        return value
    
CacheClass = SuperfluosCache if redis is None else RedisCache


valid_instance_names = [
    "harvestInstance",
    "serversInstance",
    "testInstance"
    ]


def main():
    try:
        instance_name = sys.argv[1]
    except IndexError:
        print("Please enter the name of the instance you want to log-in")
        exit(1)
        
    for valid_instance_name in valid_instance_names:
        assert isinstance(valid_instance_name, str)
        if valid_instance_name.startswith(instance_name):
            instance_name = valid_instance_name 
            break
    else:
        print("Instance name doesn't seem valid", file=sys.stderr)
        exit(1)
        
    print("Selected:", instance_name)
    cache = CacheClass()
    
    maybe_instance_ip = cache.get( instance_name )
    if maybe_instance_ip is None:
        instance_ips = findout_instance_ips.findout_instance_ips()
        for ina in valid_instance_names:
            i = instance_ips[ina]
            cache[ina] = i
        instance_ip = instance_ips[ instance_name ]
    else:
        instance_ip = maybe_instance_ip
        
    print("Instance ip:", instance_ip)
    os.execl("/usr/bin/ssh", "/usr/bin/ssh",
             # Yes, this will break most of the times....
             "-i", os.path.join( os.environ["HOME"], ".ssh/zunzun_ec2_keypair_0.pem"),
             "ubuntu@"+instance_ip
        )

if __name__ == "__main__":
    main()