namespace Models

open System
open System.Drawing
open System.Windows.Forms

module DrawModel = 
    type 'N Tree =
    | Node of 'N * 'N Tree * 'N Tree
    | Leaf of 'N
    
    let degreeToRadian degree = degree * Math.PI / 180.0
    let endPointFrom dist ang x y = float32(float(x)+dist*cos (degreeToRadian ang)), float32(float(y)+dist*sin  (degreeToRadian ang))

    let DrawLeaf (e : PaintEventArgs) (p:Pen) center radius label = 
        let (x:float32),(y:float32) = center
        let outerRectangle = RectangleF(x-radius, y-radius, radius+radius, radius+radius)
        e.Graphics.DrawEllipse(p, outerRectangle)
        e.Graphics.DrawString(label, new Font("Harrington", radius), new SolidBrush(Color.Black), outerRectangle)

    let DrawBranch (e : PaintEventArgs) (p:Pen) start length angle = 
        let x,y = start
        let endX, endY = endPointFrom length (degreeToRadian angle) x y
        e.Graphics.DrawLine(p,x,y,endX,endY)

    let DrawTree (e : PaintEventArgs) t = 
        let leafPen = new Pen(Color.Blue)
        let nodePen = new Pen(Color.Black)
        let initialCenter = 400.0f,50.0f
        let radius = 20.0f
        let drawLeafAt center label = DrawLeaf e leafPen center radius label
        let drawNodeAt center label = DrawLeaf e nodePen center radius label

        let rec loop n point depth k =
            match n with
            | Node(x,l,r) -> drawNodeAt point (string(x)) 
                             loop l (point ||> (endPointFrom (200.0/float(depth)) 135.0)) (depth+1) (fun lacc -> 
                             loop r (point ||> (endPointFrom (200.0/float(depth)) 45.0)) (depth+1) (fun racc -> 
                             () |> k))
            | Leaf(x) -> drawLeafAt point (string(x)) |> k
        loop t initialCenter 1 id

    
        
    let DrawModel = 
        let form = new Form()
        form.Size <- new Size(800,800)

        let tree = Node(1, Node(2, Leaf(3), Leaf(4)), Node(5,Leaf(6), Leaf(7)))

        form.Paint.Add (fun e -> DrawTree e tree)
        Application.Run form


