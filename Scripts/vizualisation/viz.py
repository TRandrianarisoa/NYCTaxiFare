## Greatly inspired by the zoom example

import pandas as pd
import numpy as np

cols = ["id", "fare", "date", "ilong", "ilat", "olong", "olat"]

n = 2000000
df = pd.read_csv('../../data/2009.csv.gz',
        names = cols,
        compression='gzip',
        header=-1,
        nrows=n)

# -----------------------------------------------------------------------------
# Copyright (c) 2009-2016 Nicolas P. Rougier. All rights reserved.
# Distributed under the (new) BSD License.
# -----------------------------------------------------------------------------
import numpy as np
from glumpy import app, gl, gloo

vertex = open("vertex.gl", "r").read()
fragment = open("fragment.gl").read()

ndf = df.query('ilong != 0 and ilat != 0')

n = ndf.shape[0]
v = ndf[['ilong', 'ilat']].values
v = (v  + np.array([-40.8, 74]))*5

window = app.Window(2048,2048, color=(1,1,1,1))
program = gloo.Program(vertex, fragment, count=n)
program['position'] = v # np.random.rand(n, 2)
program['zoom'] = 10.0

@window.event
def on_mouse_motion(x, y, dx, dy):
    program['mouse'] = (2.0*float(x)/window.width-1.0,
                        1.0-2.0*float(y)/window.height)

@window.event
def on_mouse_scroll(x, y, dx, dy):
    zoom = program['zoom']
    program['zoom'] = min(max(zoom *(1.0+ dy/100.0), 1.0), 50.0)

@window.event
def on_draw(dt):
    window.clear()
    program.draw(gl.GL_POINTS)

app.run()
