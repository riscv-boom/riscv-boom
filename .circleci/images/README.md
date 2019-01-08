General
-------
This DockerFile contains the necessary steps to build a Docker container that can run
projects with riscv-tools, chisel3, firrtl, and verilator. It installs the necessary
apt-get packages and sets the environment variables needed in CircleCI.

Build and Deploy the container
-----------------------------------------------------------------------------

    sudo docker build . # to test build before building it with a tag
    sudo docker build -t riscvboom/riscvboom-images:tag . # to build with tag (ex. 0.0.3)
    sudo docker login # login into the account to push to
    sudo docker push riscvboom/riscvboom-images:tag # to push to repo with tag
