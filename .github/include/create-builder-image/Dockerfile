FROM alpine:latest
RUN touch /etc/sometestfile
#FROM fpco/stack-build:latest
#RUN apt-get update
#RUN apt-get install -y python3
#RUN curl -L https://dl.bintray.com/boostorg/release/1.74.0/source/boost_1_74_0.tar.bz2 -o ~/boost_1_74_0.tar.bz2
#RUN cd ~/ && tar xjf boost_1_74_0.tar.bz2
#RUN cd ~/boost_1_74_0/ && ./bootstrap.sh --prefix=/usr --with-python=python3 && cp b2 /usr/local/bin/
#RUN cd ~/boost_1_74_0/ && b2 stage -j10 threading=multi link=static
#RUN cd ~/boost_1_74_0/ && b2 install threading=multi link=static
#RUN curl -L https://github.com/arvidn/libtorrent/releases/download/2.0/libtorrent-rasterbar-2.0.0.tar.gz -o ~/libtorrent-src.tar.gz
#RUN cd ~/ && tar xf libtorrent-src.tar.gz
##RUN git clone --depth 1 --recurse-submodules https://github.com/arvidn/libtorrent.git
#RUN apt-get install -y python3-dev libfuse-dev
#RUN cd ~/libtorrent-rasterbar-2.0.0 && LDFLAGS="-fPIE" BOOST_BUILD_PATH=~/boost_1_74_0 b2 install --prefix=/usr variant=release debug-symbols=on boost-link=static runtime-link=static dht=on link=static super-seeding=off fpic=on streaming=on cxxstd=14
