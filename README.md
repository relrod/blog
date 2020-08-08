```
docker build --cpuset-cpus="0-10" -t relrod-blog . 
docker run -it --rm  -v `pwd`:/mnt -p 8001:8000 relrod-blog /bin/bash
```
