namespace TuringDrawings

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Media
open System.Windows.Media.Imaging

module Color =
    open System.Windows.Media
    let fromRgb(r,g,b) =
        Color.FromArgb(255uy, byte r, byte g, byte b)
    let toInt (color:Color) = 
        (int color.A <<< 24) ||| 
        (int color.R <<< 16) ||| 
        (int color.G <<< 8)  ||| 
        int color.B

[<AutoOpen>]
module Symbol =
    ///Map of symbols (numbers) to colors
    let colorMap = 
        [|
        255,0  ,0      // Initial symbol color
        0  ,0  ,0      // Black
        255,255,255    // White
        0  ,255,0      // Green
        0, 0, 255      // Blue
        255,255,0  
        0  ,255,255
        255,0  ,255
        |]
        |> Array.map (Color.fromRgb >> Color.toInt)

type ViewControl () as control =
    inherit UserControl()
    let width, height = 512, 512

    let drawing = Program(numStates=4, numSymbols=3, mapWidth=width, mapHeight=height)

    let bitmap = WriteableBitmap(width,height)
    let image = Image(Source=bitmap,Stretch=Stretch.Fill,Width=float width,Height=float height) 
    do control.Content <- image

    do  async {            
        while true do
            do! Async.Sleep(10)
            drawing.Update(5000)
            let pixels = bitmap.Pixels
            drawing.Map |> Array.iteri (fun i sy -> pixels.[i] <- colorMap.[sy])
            bitmap.Invalidate()
        } |> Async.StartImmediate

type App() as app =
    inherit Application()
    do  app.Startup.AddHandler(fun o e -> app.RootVisual <- ViewControl())