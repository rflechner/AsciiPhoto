module AsciiPhoto

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

let blockSize = 8 // 8 x 8
let charset = [33 .. 126] @ [161 .. 255] |> List.map char

type PixelBrightnessAverage = float * float * int
type LettersMapping = System.Collections.Generic.IDictionary<int,char>
type RenderPixel = float -> float -> char -> unit

let blockSizeF = float blockSize
type Matrix2d = 
  { Content:int array
    N:int }
  static member Create n =
    let c = Array.create n 0
    { Content=c; N=n }
  member __.GetIndex (x:int) (y:int) = __.N * x + y
  member __.Set x y v =
    let i = __.GetIndex x y
    __.Content.[i] <- v
  member __.Get x y v =
    let i = __.GetIndex x y
    __.Content.[i]
  member __.Average () =
    __.Content |> Seq.averageBy float

let rgbToYuv(r:byte,g:byte,b:byte) =
  let y = 0.299 * float(r) + 0.587 * float(g) + 0.114 * float(b)
  let u = 0.492 * (float(b) - y)
  let v = 0.877 * (float(r) - y)
  y,u,v

let getRgb (data:JS.Uint8ClampedArray) w (x:float) (y:float) =
  let o = ((w * y) + x) * 4. |> int
  let r = data.Item o |> byte
  let g = data.Item (o+1) |> byte
  let b = data.Item (o+2) |> byte
  r,g,b

let getYuv (data:JS.Uint8ClampedArray) w (x:float) (y:float) =
  let o = ((w * y) + x) * 4. |> int
  let r = data.Item o |> byte
  let g = data.Item (o+1) |> byte
  let b = data.Item (o+2) |> byte
  rgbToYuv(r,g,b)

let getLuminanceValue (data:JS.Uint8ClampedArray) w (x:float) (y:float) c =
  let (r,g,b) = getRgb data w x y
  let r' = float r
  let g' = float g
  let b' = float b
  let i = r' * 0.3 + g' * 0.59 + b' * 0.11
  let i' = (i - 127.5) * c + 127.5
  int i'

let getDctBlocks (ctx :Browser.CanvasRenderingContext2D) c =
  let data = ctx.getImageData(0.,0.,ctx.canvas.width,ctx.canvas.height).data
  seq {
    for y in 0. .. blockSizeF .. ctx.canvas.height-1. do
      for x in 0. .. blockSizeF .. ctx.canvas.width-1. do
        let m = Matrix2d.Create blockSize
        for j in 0. .. blockSizeF-1. do
          for i in 0. .. blockSizeF-1. do
            let b = getLuminanceValue data ctx.canvas.width (x+i) (y+j) c
            m.Set (int i) (int j) b
        yield x,y,m
  } |> Seq.toList

let invertBrightness b = 
  match 255 - b with
  | v when v < 0 -> 0
  | v -> v

let render w h (averages: PixelBrightnessAverage list) (letters:LettersMapping) (renderColumn:RenderPixel) (onNewLine:unit->unit) =
  for y in 0. .. blockSizeF .. h-1. do
    for x in 0. .. blockSizeF .. w-1. do
      let (_,_,avg) = averages |> List.find(fun (x',y',v) -> (x',y') = (x,y))
      let key = invertBrightness avg
      let letter =
        if letters.ContainsKey key
        then letters.Item key
        else
          let k = letters.Keys |> Seq.minBy(fun k -> k > key)
          letters.Item k
      renderColumn x y letter
    onNewLine()

let getPixelCountForLetter (ctx:Browser.CanvasRenderingContext2D) (letter:char) w h =
  ctx.clearRect(0.,0.,w,h)
  ctx.fillText(letter.ToString(), 0., h/2.)
  let data = ctx.getImageData(0.,0.,w-1.,h-1.).data
  seq {
    for i in 0. .. data.length - 1. do
      if data.Item (int i) <> 0.
      then yield true
  } |> Seq.length

let computeLettersWeights (alphabet:char list) =
  let charPreview = Browser.document.getElementById "charPreview" :?> Browser.HTMLCanvasElement
  let ctx = charPreview.getContext_2d()
  ctx.clearRect(0.,0.,charPreview.width,charPreview.height)
  charPreview.width <- 30.
  charPreview.height <- 30.
  ctx.fillStyle <- !^"rgb(255,255,255)"
  ctx.fillRect (0., 0., 50., 50.)
  ctx.fillStyle <- !^"black"
  ctx.font <- "20px Courier New"
  let result =
    alphabet
    |> List.map (fun l -> (getPixelCountForLetter ctx l charPreview.width charPreview.height),l)
    |> List.distinctBy fst
    |> dict
  charPreview.remove()
  result

let getElementById<'t when 't :> Browser.HTMLElement > id =
  Browser.document.getElementById id :?> 't

let drawAnalysis (averages: (float*float*int) list) h w (srcCtx:Browser.CanvasRenderingContext2D) (dstCtx:Browser.CanvasRenderingContext2D) =
  dstCtx.clearRect(0.,0.,w,h)
  dstCtx.fillStyle <- !^ "white"
  dstCtx.fillRect(0.,0.,w,h)
  dstCtx.fillStyle <- !^"black"
  dstCtx.strokeStyle <- !^"gray"
  dstCtx.font <- "9px arial"
  for x in 0. .. blockSizeF .. w-1. do
    for y in 0. .. blockSizeF .. h-1. do
      let (_,_,avg) = averages |> List.find(fun (x',y',v) -> (x',y') = (x,y))
      dstCtx.fillStyle <- !^(sprintf "rgb(%d,%d,%d)" avg avg avg)
      dstCtx.fillRect(x,y, blockSizeF, blockSizeF)

let init() =
  let contrastSlider = getElementById<Browser.HTMLInputElement> "contrast"
  let widthSlider = getElementById<Browser.HTMLInputElement> "width"
  let heightSlider = getElementById<Browser.HTMLInputElement> "height"
  let loading = getElementById<Browser.HTMLDivElement> "loading"
  let canvas = getElementById<Browser.HTMLCanvasElement> "canvas"
  let analyzer = getElementById<Browser.HTMLCanvasElement> "analyzer"
  let contrastDisplay = getElementById<Browser.HTMLSpanElement> "contrastDisplay"
  let widthDisplay = getElementById<Browser.HTMLSpanElement> "widthDisplay"
  let heightDisplay = getElementById<Browser.HTMLSpanElement> "heightDisplay"
  let fileField = getElementById<Browser.HTMLInputElement> "file"
  let computeButton = getElementById<Browser.HTMLButtonElement> "computeButton"

  let lettersWeights = 
    charset
    |> List.append [ ' ' ]
    |> computeLettersWeights

  contrastDisplay.innerText <- contrastSlider.value

  contrastSlider.addEventListener_change(fun _ ->
    contrastDisplay.innerText <- contrastSlider.value
    unbox true)
    
  widthSlider.addEventListener_change(fun _ ->
    widthDisplay.innerText <- widthSlider.value + "px"
    canvas.width <- widthSlider.valueAsNumber
    analyzer.width <- canvas.width
    unbox true)

  heightSlider.addEventListener_change(fun _ ->
    heightDisplay.innerText <- heightSlider.value + "px"
    canvas.height <- heightSlider.valueAsNumber
    analyzer.height <- canvas.height
    unbox true)

  canvas.width <- widthSlider.valueAsNumber
  canvas.height <- heightSlider.valueAsNumber
  let ctx = canvas.getContext_2d()
  ctx.fillStyle <- !^"black"
  ctx.font <- "14px Courier New"
  analyzer.width <- canvas.width
  analyzer.height <- canvas.height

  let ratio max value = 254 * value / max
  let max = lettersWeights.Keys |> Seq.max
  let letters =
    seq {
      for k in lettersWeights.Keys |> Seq.sort do
        let v = lettersWeights.Item k
        let r = ratio max k
        yield r,v
    } |> Seq.distinctBy fst
      |> dict

  let imageLoader = Browser.document.createElement_img()

  computeButton.addEventListener_click(fun _ -> 
    loading.hidden <- false

    if fileField.files.length >= 1.
    then
      let file = fileField.files.Item 0
      let url = Browser.URL.createObjectURL file
      imageLoader.src <- url
    else
      loading.hidden <- true

    unbox true)

  imageLoader.addEventListener_load(
      fun _ ->
        ctx.clearRect(0.,0.,canvas.width,canvas.height)
        analyzer.getContext_2d().clearRect(0.,0.,canvas.width,canvas.height)

        let canvasImg = U3.Case2(imageLoader :?> Browser.HTMLCanvasElement)
        let sw = canvas.width * canvas.width / canvas.height
        let sh = canvas.height * canvas.width / canvas.height
        ctx.drawImage(canvasImg, 0., 0., imageLoader.width, imageLoader.height, 0., 0., sw, sh)
        
        let c = contrastSlider.valueAsNumber
        let matrices = getDctBlocks ctx c
        let averages = matrices |> List.map(fun (x,y,m) -> x,y,(m.Average() |> int))
        
        ctx.clearRect(0.,0.,canvas.width,canvas.height)

        analyzer.getContext_2d() |> drawAnalysis averages analyzer.height analyzer.width ctx

        ctx.fillStyle <- !^"black"
        ctx.strokeStyle <- !^"gray"
        ctx.font <- "9px arial"

        render canvas.width canvas.height averages letters (fun x y letter -> ctx.fillText(letter.ToString(), x, y)) ignore
        
        let txt = getElementById<Browser.HTMLInputElement> "txt"
        txt.value <- ""
        render canvas.width canvas.height averages letters 
          (fun _ _ letter -> txt.value <- txt.value + letter.ToString())
          (fun _ -> txt.value <- txt.value + "\n")
        
        loading.hidden <- true
        unbox true)
  loading.hidden <- true
  
init()

