Steps to build a Docker container that has the necessary tools for riscv-boom
-----------------------------------------------------------------------------

sudo docker build . # to test
sudo docker build -t riscvboom/riscvboom-images:tag . # to build with tag (ex. 0.0.3)
sudo docker login
sudo docker push riscvboom/riscvboom-images:tag # to push to repo with tag
