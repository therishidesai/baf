# baf: big ass fan

A pub/sub system built on top of the
[/dev/fanout](https://github.com/bob-linuxtoys/fanout/tree/master)
kernel module

## What is /dev/fanout?

/dev/fanout is a kernel module that makes a linux character device act
as a one to many ring buffer. This is a great building block for an
efficient pub/sub mechanism.

## How does baf work?

baf has 3 main components: nixos module, pubmsg, and submsg

### nixos module

The nixos module sets up a oneshot systemd unit that statically sets
up the system. You can import the module into your NixOS config and
use it by enabling it and then setting a list of topics:

```
{
  baf.services.baf-setup = {
    enable = true;
    topics = [ "topic1" "topic2" ];
  };
}
```

For a more detailed example look at `baf-test` inside of the flake.nix
in this repo.

**NOTE: A `baf-test` topic will always be added for debugging**

### pubmsg

`pubmsg <topic-name>` is the simple program that a publisher can
use. It will use the topic name to find the correct `/dev/baf` symlink
and then will pipe any data on stdin to the `/dev/baf` device. If the
topic is already in use or `/dev/baf` file doesn't exist it will exit.

Example usage:

``` service-x | pubmsg test1 ```

This will take the stdout of `service-x` and send it to any program
reading off of the `/dev/baf` character device.

For a more detailed example look at `baf-test` inside of the flake.nix
in this repo.

### submsg (TODO)

`submsg` is the program a subscriber can use to get data. With no
args, `submsg` will subscribe to every topic and feed data to a
publisher via stdin. A user can provide a list of topics, `submsg
[TOPICS]`, to only get data from those `/dev/baf` character devices.

Example 
```
submsg topic1 topic2 | service-y
```

This will take data from topic1 and topic2 and send it to `service-y`
on stdin.

For a more detailed example look at `baf-test` inside of the flake.nix
in this repo.


## TODO

- add topic filtering to submsg
- remove the need for statically defining the system
  - create the `/dev/baf` devices dynamically when pubmsg or submsg gets ran
- improve submsg throughput
- add tests for correctness and perf