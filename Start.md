### Introduction ###

The purpose of building the exvm virtual machine is learning. Or understanding. Understanding how computers work and why they work this way. The purpose of the virtual machine itself is also learning. Or understanding. Understanding how software work and why it works this way. I plan to bootstrap a compiler for the VM, try to build a (quick and dirty) OS and more. You know what? Forget the above, the real purpose is having fun!

### Design Guidelines and Requirements ###

  * Keep implementation as simple as possible. This does not refer to VM features, but rather to the way they are implemented. Of course, the VM will have no features that are not needed.
  * The VM should be somewhat 'realistic', that is, programming for it needs to be similar to programming for a real (commodity) hardware.

### Design ###

  * Architecture
  * Instruction Set