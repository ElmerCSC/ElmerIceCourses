from paraview.simple import *
"""
A Collection of function for paraview to plot the grounding line
- GLFlot: apply flotation and plot 0-isocontour
- GLEdges: extract GL edges
"""

def GLFlot(rhoi=910.0,rhow=1028.0,zsea=0.0):
    """Plot GL from Flotation
    Input: 
        - rhoi:ice density
        - rhow: water density
        - zsea: sea level
    """
    Source = GetActiveSource()
    Source.UpdatePipeline()
    View = GetActiveViewOrCreate('RenderView')
    
    # compute flotation depth - bed
    Flot = Calculator(Input=Source)
    Flot.ResultArrayName = 'Flotation'
    Flot.Function = '{}-h*{}/{}-bedrock'.format(zsea,rhoi,rhow)
    Flot.UpdatePipeline()

    # Extract 0-isocontour
    contour1 = Contour(Input=Flot)
    contour1.ContourBy = ['POINTS', 'Flotation']
    contour1.Isosurfaces = [0.0]
    contour1.UpdatePipeline()

    # show data in view
    CDisplay = Show(contour1, View)
    ColorBy(CDisplay, None)
    CDisplay.AmbientColor = [1.0, 0.3333333, 1.0]
    CDisplay.DiffuseColor = [1.0, 0.3333333, 1.0]


def GLEdges():
    """Plot GL from element Edges
    """
    Source = GetActiveSource()
    Source.UpdatePipeline()

    View = GetActiveViewOrCreate('RenderView')

    # Extract floting elment (at least one mask<0)
    threshold1 = Threshold(Input=Source)
    threshold1.Scalars = ['POINTS', 'groundedmask']
    threshold1.ThresholdRange = [-1.0, -0.5]
    threshold1.AllScalars = 0
    threshold1.UpdatePipeline()

    # Extract Element Edges
    extractEdges1 = ExtractEdges(Input=threshold1)
    extractEdges1.UpdatePipeline()

    # Extract GL Edges
    threshold2 = Threshold(Input=extractEdges1)
    threshold2.Scalars = ['POINTS', 'groundedmask']
    threshold2.ThresholdRange = [0.0, 0.0]
    threshold2.UpdatePipeline()

    # show data in view
    threshold2Display = Show(threshold2, View)
    # turn off scalar coloring
    #ColorBy(threshold2Display, None)
    # change solid color
    threshold2Display.AmbientColor = [1.0, 1.0, 0.0]
    threshold2Display.DiffuseColor = [1.0, 1.0, 0.0]




