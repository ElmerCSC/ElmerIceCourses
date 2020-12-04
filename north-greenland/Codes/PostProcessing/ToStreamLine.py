from paraview.simple import *
"""
A Collection of function for paraview to interpolate
results on a streamLine
- Resample: Interpolate active source on streamLine
- SaveResampled: Save as csv
- FlowLinePlot: Make a new layout with flow line plot
"""


def Resample(StreamFile):
    """Interpolate to StreamLine
    Input: 
        - StreamFile: csv file, comma delimited with header
          required variables:
          - coordinates (Points:0,Points:1,Points:2)
          - horizontal velocity (vx,vy)
    Resample the active Source to the StreamLine points
    """

    View = GetActiveViewOrCreate('RenderView')

    # Get Active Source
    Source = GetActiveSource()
    Source.UpdatePipeline()

    # Read the StreamLine
    streamLine = CSVReader(FileName=StreamFile)
    streamLine.UpdatePipeline()

    # Covert the table to Points
    tableToPoints1 = TableToPoints(Input=streamLine)
    tableToPoints1.XColumn = 'Points:0'
    tableToPoints1.YColumn = 'Points:1'
    tableToPoints1.ZColumn = 'Points:2'
    tableToPoints1.a2DPoints = 1
    tableToPoints1.UpdatePipeline()

    # Make a vector for vx,vy
    Vobs = Calculator(Input=tableToPoints1)
    Vobs.ResultArrayName = 'Vobs'
    Vobs.Function = 'vx*iHat+vy*jHat'
    Vobs.UpdatePipeline()

    # Interpolate Active source on StreamLine
    resampleWithDataset1 = ResampleWithDataset(SourceDataArrays=Source,DestinationMesh=Vobs)
    resampleWithDataset1.PassPointArrays = 1
    resampleWithDataset1.UpdatePipeline()
    RenameSource( proxy=resampleWithDataset1 , newName = "Resampled")

    # Show the StreamLine
    Display = Show(resampleWithDataset1,View)
    Render(View)

def SaveResampled(FileName):
    """Save Resampled results in csv
    Input: 
        - FileName: Name of output File
    Save the resampled source as csv
    Save all time steps
    """
    resampled = FindSource('Resampled')
    # save data
    SaveData(FileName, proxy=resampled, WriteTimeSteps=1,Precision=8,UseScientificNotation=1)

def FlowLinePlot(Vsize=[800, 400]):
    """
    Create a new layout to plot data along the flow line
    """

    layout=CreateLayout('FlowLine Layout')
    layout.SplitVertical(0, 0.5)

    # Create a new 'Line Chart View'
    lineChartView1 = CreateView('XYChartView')
    lineChartView1.ViewSize = Vsize
    AssignViewToLayout(view=lineChartView1, layout=layout, hint=0)

    # Create a new 'Line Chart View'
    lineChartView2 = CreateView('XYChartView')
    lineChartView2.ViewSize = Vsize
    AssignViewToLayout(view=lineChartView2, layout=layout, hint=2)

    # get layout
    #layout2 = GetLayoutByName("Layout #2")

    # find source
    resampled = FindSource('Resampled')

    # create a new 'Plot Data'
    plotData1 = PlotData(Input=resampled)

    # show data in view
    plotVelocity = Show(plotData1, lineChartView1, 'XYChartRepresentation')
    plotVelocity.SeriesVisibility = ['ssavelocity_Magnitude', 'Vobs_Magnitude']
    plotVelocity.SeriesColor = ['ssavelocity_Magnitude', '0', '0', '0','Vobs_Magnitude', '0.889998', '0.100008', '0.110002']

    # show data in view
    plotTopo = Show(plotData1, lineChartView2, 'XYChartRepresentation')
    plotTopo.SeriesVisibility = ['bedrock', 'zb', 'zs']
    plotTopo.SeriesColor = ['bedrock', '0', '0', '0', 'zb', '0', '0', '1','zs', '0', '0', '1']


