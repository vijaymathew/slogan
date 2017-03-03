A simple package for graphics programming.

Remember to install the SDL2 dependencies before building the package:

### Ubuntu

```
sudo apt-get install libsdl2-dev libsdl2-image-dev \
libsdl2-mixer-dev libsdl2-ttf-dev libjpeg-dev libpng12-dev libsdl2-gfx-dev
```

### Mac OS X

```
brew install sdl2 sdl2_gfx sdl2_image sdl2_mixer sdl2_ttf
```

Install the package with this command:

```
$ slogan -i "media,local,/path_to_slogan/example/packages/media"
```

See the `sample` folder for code examples.

Here is how you may run one of the samples from the Slogan REPL:

```
$ cd example/packages/media
$ slogan

slogan> load_package("media");
slogan> reload("./sample/bouncing_ball");
```