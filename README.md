DEPRECATED oc_bifrost
========

# DEPRECATED: Moved to chef-server repository

New development on this component of the Chef Server now happens
in the Chef Server repository:

https://github.com/chef/chef-server

This repository should only be used for changes to older versions
of Chef Server.

## License

All files in the repository are licensed under the Apache 2.0 license. If any
file is missing the License header it should assume the following is attached;

```
Copyright 2014 Chef Software Inc

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
```

Bifrost?
=========

`oc_bifrost` is the Opscode Authorization API server.  It is named
after [Bifrost][], the burning rainbow bridge to Asgard in Norse mythology.

`oc_bifrost` is a complete rewrite and replacement of the old
[opscode-authz][] API server.

[Bifrost]:http://en.wikipedia.org/wiki/Bifrost
[opscode-authz]:https://github.com/opscode/opscode-authz

Testing and Development
=======================

We're using [Berkshelf][] to make things easy.

First, you'll need to configure Berkshelf to talk to our preprod Chef
Server.  Create or add to a `~/.berkshelf/config.json` file the
following information:

``` javascript

{
    "chef": {
      "chef_server_url": "https://opsmaster-api.opscode.us/organizations/preprod",
      "node_name": <YOUR_OPSMASTER_ACCOUNT_NAME>,
      "client_key": <PATH_TO_YOUR_OPSMASTER_SSH_KEY>
    },
    "ssl": {
      "verify": false
      }
    }
}
```

The `ssl` directive is important!

Also, while we still have a monolithic [chef repo][], we'll need to
refer to our platform roles and data bags in order to replicate our
production environment as much as possible in a local Vagrant setting.
The easiest way to make this work is to set an environment variable
that points to a local checkout of the platform cookbooks repo, which
the [Vagrantfile](Vagrantfile) then uses.

```
export OPSCODE_PLATFORM_REPO=/path/to/local/checkout/of/repo
```

The following environmental variable is used for locating local cookbooks
(however, even if no local cookbooks are used and the variable is empty or
pointing at an empty directory, it's still required):

    export OPSCODE_COOKBOOKS=/path/to/working/cookbooks

Also, the following environmental variable must point to all the opscode
cookbooks being worked on (i.e., oc_bifrost, etc. which are mounted into
the VM):

    export OPSCODE_SRC=/path/to/src/oc/

Now you're ready to grab all the dependencies.  We're installing
binary stubs into `bin` to ensure everything is as self-contained as
possible.

The `Vagrantfile` will only work for the latest installer-based
versions of Vagrant.  If you do not already have this, please download
it from http://downloads.vagrantup.com.  Note that it __will not
work__ with earlier gem-based versions of Vagrant!  Please see
[these instructions](doc/vagrant.md) for help in transitioning.

You'll need to set up the Berkshelf plugin for Vagrant:

    /usr/bin/vagrant plugin install vagrant-berkshelf

You will also need the Omnibus plugin, to ensure that we have an
up-to-date version of Chef on the VMs:

    /usr/bin/vagrant plugin install vagrant-omnibus

Now, to fire up and provision all the VMs:

```
/usr/bin/vagrant up
```

We currently have 3 VMs:

- `db`: the database server
- `api`: the API server
- `metrics`: an estatsd / graphite / gdash server, useful for
  developing graphs for Bifrost.

Remember: you can interact with each VM individually by providing its name, e.g.:

    /usr/bin/vagrant up db

Go muck around on a specific machine now:

    /usr/bin/vagrant ssh db

To re-run `chef-client` on your test machine:

    /usr/bin/vagrant provision db

If you screw something up horribly, just destroy the machine and start
again:

    /usr/bin/vagrant destroy db

Eventually, we'll be adding Test Kitchen support for running
[oc-bifrost-pedant][] and our pgTAP database schema tests.

Cookbook Hacking
================

If you want to hack on the [opscode-bifrost][] cookbook, you'll need to
make a minor tweak to the Berksfile.

First, perform a local checkout of the cookbook.  Then, modify the
dependency line in `Berksfile`.

Change this:

``` ruby
cookbook "opscode-bifrost, git: "git@github.com:opscode-cookbooks/opscode-bifrost"
```

to this:

``` ruby
cookbook "opscode-bifrost", path: "/path/to/local/checkout/of/opscode-bifrost"
```

Re-provision your machine and you'll be running off the local version
of the cookbook.

[Berkshelf]:http://berkshelf.com
[oc-bifrost-pedant]:https://github.com/opscode/oc-bifrost-pedant
[opscode-bifrost]:https://github.com/opscode-cookbooks/opscode-bifrost
[chef repo]:https://github.com/opscode/opscode-platform-cookbooks

Cutting A Release
=================

See [Cutting a New Release of Bifrost](doc/release.md) for the gory details.
