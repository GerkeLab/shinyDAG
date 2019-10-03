## Building and developing shinyDAG

### shinyDAG dev environment

I use the Dockerfile in this folder to create a fully-featured RStudio docker container that is *pretty close* to the final shinyDAG environment.
Note that it's not perfect and if you install packages into this container, you'll need to also update the main shinyDAG docker file [here](../Dockerfile).

To create the dev environment:

```bash
# make sure you're in the ./dev folder
cd dev

# make the dev image
docker build -t shinydag-dev .

# move back to shinyDAG proper and start up the dev image
cd ..
docker run --rm -d -p 8787:8787 -v $(pwd):/home/rstudio/shinydag -e PASSWORD="password" shinydag-dev
```

(Note that you should probably change the password above, but if you're only running locally it's not a big deal.)

Then navigate to <localhost:8787> and login using the password you entered.

### Create shinyDAG image

```bash
docker build -t gerkelab/shinydag:dev .

# To send up to docker hub
docker push
```

The `:dev` indicates that this image is tagged `dev`, but this can be anything you want.
If you don't add a tag, it's assumed to be `:latest` which is kind of like git's `master` but for docker containers.
