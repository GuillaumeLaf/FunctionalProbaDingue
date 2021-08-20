namespace Models

// https://llimllib.github.io/pymag-trees/

open System
open System.Drawing
open System.Windows.Forms

module DrawStructure = 
    
    let drawOperator (e:PaintEventArgs) (p:Pen) op (center:float*float) (radius:float) = 
        let pen = new Pen(Color.Black)
        let x,y = center
        match op with
        | Addition -> e.Graphics.DrawLine(pen, float32(x-radius/2.0), float32(y), float32(x+radius/2.0), float32(y))
                      e.Graphics.DrawLine(pen, float32(x), float32(y-radius/2.0), float32(x), float32(y+radius/2.0))
        | Multiplication -> e.Graphics.DrawLine(pen, float32(x-radius/2.0), float32(y+0.5), float32(x+radius/2.0), float32(y+0.5))
                            e.Graphics.DrawLine(pen, float32(x-radius/2.5), float32(y-radius/2.5), float32(x+radius/2.5), float32(y+radius/2.5))
                            e.Graphics.DrawLine(pen, float32(x+radius/2.5), float32(y-radius/2.5), float32(x-radius/2.5), float32(y+radius/2.5))
        | _ -> ()

    let graphHeigth = Graph.Skeleton.height
    
    let degreeToRadian degree = degree * Math.PI / 180.0
    let radianToDegree radian = radian * 180.0 / Math.PI
    let endPointFrom dist ang x y = float(x)+dist*cos (degreeToRadian ang), float(y)+dist*sin  (degreeToRadian ang)
    let angleBetween (x1,y1) (x2,y2) = atan2 (y2-y1) (x2-x1) * 180.0 / Math.PI
                                       
    let DrawLeaf (e : PaintEventArgs) (p:Pen) (center:float*float) (radius:float) label = 
        let x,y = center
        e.Graphics.DrawEllipse(p, RectangleF(float32(x-radius), float32(y-radius), float32(radius+radius), float32(radius+radius)))
        e.Graphics.DrawString(label, new Font("Arial", float32(radius*1.3)), new SolidBrush(Color.Black), RectangleF(float32(x-radius*0.75), float32(y-radius), float32(radius+radius), float32(radius+radius)))

    let DrawBranch (e : PaintEventArgs) (p:Pen) (source:float*float) (destination:float*float) = 
        let x,y = source
        let endX, endY = destination
        e.Graphics.DrawLine(p,float32(x),float32(y),float32(endX),float32(endY))

    let DrawGraph (e : PaintEventArgs) skeleton = 
        let maxWidthOfTree = 10.0*(2.0**float((graphHeigth skeleton - 1)))
        let initialY = 50.0
        let initialX = 50.0 + maxWidthOfTree

        let nodePen = new Pen(Color.Black,2.0f)
        let innovationPen = new Pen(Color.Gold,2.0f)
        let parameterPen = new Pen(Color.Blue,2.0f)
        let variablePen = new Pen(Color.Red,2.0f)
        let constantPen = new Pen(Color.Orange,2.0f)
        let branchPen = new Pen(Color.Black,2.0f)

        let initialCenter = initialX,initialY
        let radius = 10.0

        let drawLeafAt p center label = DrawLeaf e p center radius label
        let drawNodeAt center label = DrawLeaf e nodePen center radius label
        // let drawOperatorAt op center  = drawOperator e nodePen op center radius

        let rec loop n point depth k = 
            let currentX, currentY = point
            let currentShift = maxWidthOfTree/(float(depth))
            let leftPoint = currentX - currentShift/2.0, float(depth+1)*50.0
            let rightPoint = currentX + currentShift/2.0, float(depth+1)*50.0
            match n with
            | Node(op,left,right) -> DrawBranch e branchPen (endPointFrom radius (angleBetween point leftPoint) currentX currentY) (endPointFrom radius (angleBetween leftPoint point) (fst leftPoint) (snd leftPoint))
                                     DrawBranch e branchPen (endPointFrom radius (angleBetween point rightPoint) currentX currentY) (endPointFrom radius (angleBetween rightPoint point) (fst rightPoint) (snd rightPoint))
                                     match op with
                                     | Addition -> drawNodeAt point ("+"); loop left leftPoint (depth+1) (fun lacc -> loop right rightPoint (depth+1) (fun racc -> () |> k))
                                     | Multiplication -> drawNodeAt point ("*"); loop left leftPoint (depth+1) (fun lacc -> loop right rightPoint (depth+1) (fun racc -> () |> k))
                                     | Substraction -> drawNodeAt point ("-"); loop left leftPoint (depth+1) (fun lacc -> loop right rightPoint (depth+1) (fun racc -> () |> k))
                                     | LessThan -> drawNodeAt point ("<"); loop left leftPoint (depth+1) (fun lacc -> loop right rightPoint (depth+1) (fun racc -> () |> k))
            | Leaf(input,x,idx,_) -> match input with
                                     | Innovation -> (drawLeafAt innovationPen point (string(idx))) |> k   // input arrays must at least be the same length as the number of innovation nodes.
                                     | Parameter -> (drawLeafAt parameterPen point (string(idx))) |> k
                                     | Variable -> (drawLeafAt variablePen point (string(idx))) |> k
                                     | PreviousResult -> (drawLeafAt nodePen point (string(idx))) |> k
                                     | Constant -> (drawLeafAt constantPen point (string(idx))) |> k
        loop skeleton initialCenter 1 id
  
        
    let Model (T(name,Graph(_,skeleton),_)) = 
        let form = new Form()
        form.Size <- new Size(1600,800)

        form.Paint.Add (fun e -> DrawGraph e skeleton)
        Application.Run form


