from paraview.simple import *
import sys
sys.path.append('../../Codes/PostProcessing')

from ToStreamLine  import *
from PlotGroundingLine import *

"""
Simple script to show how to use pvpython to automatise
post processing
"""

StreamLine="../../Data/StreamLine/StreamLine.csv"
SaveFile="ResOnStreamLine.csv"
vtuFile='MESH_1/RUN1__t0001.vtu'


# read vtu file 
vtu = XMLUnstructuredGridReader(FileName=vtuFile)

# Show results
renderView1 = GetActiveViewOrCreate('RenderView')
renderView1.ViewSize = [600, 600]
Display = Show(vtu, renderView1)

# set scalar coloring
ColorBy(Display, ('POINTS', 'ssavelocity', 'Magnitude'))
ssavelocityLUT = GetColorTransferFunction('ssavelocity')
ssavelocityLUT.MapControlPointsToLogSpace()
ssavelocityLUT.UseLogScale = 1
ssavelocityLUT.RescaleTransferFunction(1.0, 2000.0)

## plot Grounding line edges
SetActiveSource(vtu)
GLEdges()

## plot Grounding line from flotation
SetActiveSource(vtu)
GLFlot()

## resample stream Line
SetActiveSource(vtu)
Resample(StreamLine)

## save resampled
SaveResampled(SaveFile)

## Save screenshot
# current camera placement for renderView1
renderView1.InteractionMode = '2D'
renderView1.CameraPosition = [-249524.47156507644, -1022008.0234601084, 10000.0]
renderView1.CameraFocalPoint = [-249524.47156507644, -1022008.0234601084, 0.0]
renderView1.CameraParallelScale = 226895.9823084111

SaveScreenshot('map.png')

## make pv 2D plots
FlowLinePlot()

## save screenshot
layout = GetLayoutByName("FlowLine Layout")   
SaveScreenshot('Stream.png', layout, SaveAllViews=1)
