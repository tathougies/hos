Hos: A Haskell operating system
--------------------------------

The kernel is written in mainly JHC Haskell, with fallbacks to C where necessary (biggest one being the garbage collector and rts). JHC was chosen because it produces ANSI C code which is easy to compile with a cross-compiler.

There are some bugs with monad transformers in JHC which make the compiler crash. You may see a funny idiom where we bind monadic calls to variables x1, x2, etc. and then `seq` them together before returning. This prevents an over-eager optimization from running which messes up the compile. At some point, I will spend time to patch JHC, but until then, the code compiles and seems to run.

Dependencies
------------

- Latest version of the JHC compiler.
- GNU Xorriso (tested on 1.3.8)
- GNU compatible make
- A clang cross-compiler
- GNU binutils (needed to run ld linker scripts)

Build System
-------------

The regular JHC make system is useless to us, because it wasn't designed to compile operating systems (duh!). Instead, we use the JHC -C flag to create ANSI C sources from the kernel and user-space haskell sources. We then compile these sources using a clang cross-compiler, and finally link them together to produce the output.

If you have everything necessary to build, modify build.opts to make sure everything points to the right locations, and type make at the top-level. Get a cup of tea, because JHC takes its time.

Run it
------

The easiest way to run Hos for testing is to use the supplied ISO. Hos has been tested only on VirtualBox. Currently, it only produces output via the serial port, which isn't very exciting, but it is what it is. If someone would like to write a text or graphics subsystem, we're looking for volunteers!

Steps to set it up with VirtualBox:

1. Open VirtualBox, and hit 'New'
2. Name the virtual machine, choose 'Other' as the type, and choose 'Other/Unknown (64-bit)' as the version.
3. Give it at least 256 MB to play with.
4. Create a new hard drive in any format, and make it 2GB in size. WE currently don't use it but our ATA driver presumes the existence of one
5. Hit settings to set the serial port information.
6. Open the 'ports' pane and select 'Enable serial port' for COM1. The IRQ should be 4 and the port should be 0x3f8
7. Change 'Port Mode' to 'Raw file' and set 'Port/File Path' to a suitable output file location (you'll have to type it in).
7. Hit run. Since your VM has no hard drive, VirtualBox should prompt you to choose an ISO
8. Hit the browse button and choose the hos.iso from github
9. Open your terminal and run 'tail -f <name-of-serial-port-output>'
10. Hit enter in the virtual box window, if all goes well you should see text streaming out of the file. If all goes well it should end with a message that the kernel has completed.

Voilà, you've run a Haskell kernel!
