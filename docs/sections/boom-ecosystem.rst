The BOOM Ecosystem
==================

Scala, Chisel, Generators, Configs, Oh My!
------------------------------------------

Working with BOOM can be a bit confusing the first time because you
have to learn about the micro-architecture, Rocket-chip, Chisel (maybe Firrtl), Scala, and the 
build system. The micro-archicture is detailed here in this documentation,
while the other topics are discussed in their respective websites and courses (except for Rocket-chip
which is notoriously undocumented).
This section hopes to get you a quick high level view on the entire build process
(once you have a design, how do you get from Chisel/Scala to Verilog). Note that this
section explains how things work in `boom-template <https://github.com/riscv-boom/boom-template>`__ .

Coding in Scala/Chisel
~~~~~~~~~~~~~~~~~~~~~~

When making changes to BOOM, you are working in Scala/Chisel. Chisel is the language to 
create RTL while Scala is used to parameterize your design. For example, if you want to make
a queue, you would use Chisel to make a queue. However, if you want to change the amount of entries
of the queue based on some variable, that would be Scala code. Another way to think of the distinction is
that Chisel code will make a circuit in hardware while Scala code will change that circuits parameters.

BOOM Generator
~~~~~~~~~~~~~~

The word "generator" used in many Chisel projects just refers to a program that takes in parameters
and a module top and returns a circuit based on those parameters. For ex, with BOOM, the parameters are specified in the 
configurations that it uses. Parameters are just a set of Scala variables. Those configurations change the size of things like queues, how many there are,
etc. The module top would be something like the "ExampleBoomSystem" which would describe the parameterized circuit 
that you are trying to build. So to build something like BOOM, you need a config (set of Scala parameters) and a module top (ExampleBoomSystem) to
pass into the generator. The generator will take these two items and combine them to form a piece of RTL represting the circuit given in the module top.

Note: Technically, if you are using boom-template, the module top is "TestHarness" which within it instantiates "ExampleBoomSystem"

Compilation and Elaboration
~~~~~~~~~~~~~~~~~~~~~~~~~~~

In order to run the generator, all Scala/Chisel sources must be built. Thus, the Scala/Chisel files are compiled into \*.jars. If you think
of Chisel as a libary within Scala, then these classes being built are just Scala classes which call Chisel functions. Thus, any errors that you 
get in compiling the Scala/Chisel files are errors that you are not connecting the Scala and Chisel code together correctly (similar to if you misuse
library functions and you get a compiler error). This building of \*.jars is **compilation**. After all the classes are compiled, **elaboration** begins.
Elaboration is where the Chisel library functions are called with the parameters given and Chisel tries to construct a circuit based on what you wrote
in Chisel code. If an error happens here, this is a runtime error where Chisel cannot "build" your circuit. However, if that passes, the output of the
generator gives you an RTL file!

Quickly on Firrtl
~~~~~~~~~~~~~~~~~

Up until this point, I have been saying that your generator gives you a RTL file. However... this is not true. Instead the generator emits
Firrtl (website), an intermediate representation of your circuit. Without going into Firrtl too much, this Firrtl is consumed by a Firrtl
compiler which then creates the Verilog file after doing a series of optimization passes (and other passes). You might notice this optimization
pass by seeing the signals from the RTL and seeing signals like _T_123.

Big Picture
~~~~~~~~~~~

Now that the flow of ecosystem has been briefly explained here is a quick recap. You first code in Scala/Chisel (where Chisel can be seen as a library that
Scala uses). This code is converted to Firrtl (an intermediate representation of the circuit). The code is then converted to Verilog. This process is done
by creating a generator that takes in the module top of the project and all the parameters (configs) that it needs. At each point in this process you can get
errors.

You write Scala + Chisel.
You compile the Scala + Chisel into a Generator (a Scala program with the "Chisel functions")
    Errors occur here that you messed up the "interface between Scala and Chisel"
You run the generator (Chisel generator)
    This can give you Chisel errors (runtime errors) that you can't build the circuit (aka the firrtl)
Then you run the firrtl compiler on the output firrtl (another Scala program) to get verilog
    You can get errors there that the firrtl cannot build a circuit (runtime errors)
