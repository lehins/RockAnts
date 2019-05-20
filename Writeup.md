# Developent stages


# Stage 0

## Initialize the project

```
$ stack new RockAnts lehins --bare
```

Create module structure:

```
src
├── RockAnts
│   ├── Grid.hs
│   ├── Model.hs
│   └── Types.hs
└── RockAnts.hs
```

# Stage 1

Create a grid from nests and grid spec

# Stage 2

* Abstract the same pattern as in previous stage

* Drawing different hexagons

* Write cellDrawer that will create an image with a grid of hexagonal cells.

# Stage 3

* Draw a grid on the image.

* Write `zoomWithGridD` and `zoomWithGrid`

# Stage 4

* Initialize colony

* Draw colony on the grid image

# Stage 5

* Make a worker move

# Stage 6

* create a cycle and produce an animation
