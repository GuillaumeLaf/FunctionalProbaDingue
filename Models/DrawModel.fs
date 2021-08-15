namespace Models

open System
open System.Drawing
open System.Windows.Forms

module DrawModel = 
    type 'N Tree =
    | Node of 'N * 'N Tree * 'N Tree
    | Leaf of 'N

    let treeHeight tree = 
        let rec loop t k = 
            match t with 
            | Node(x,l,r) -> loop l (fun lacc -> 
                             loop r (fun racc -> 
                             (1 + max lacc racc) |> k))
            | Leaf(x) -> 1 |> k
        loop tree id
    
    let degreeToRadian degree = degree * Math.PI / 180.0
    let endPointFrom dist ang x y = float32(float(x)+dist*cos (degreeToRadian ang)), float32(float(y)+dist*sin  (degreeToRadian ang))

    let DrawLeaf (e : PaintEventArgs) (p:Pen) (center:float*float) (radius:float) label = 
        let x,y = center
        let outerRectangle = RectangleF(float32(x-radius), float32(y-radius), float32(radius+radius), float32(radius+radius))
        e.Graphics.DrawEllipse(p, outerRectangle)
        e.Graphics.DrawString(label, new Font("Harrington", float32(radius)), new SolidBrush(Color.Black), outerRectangle)

    let DrawBranch (e : PaintEventArgs) (p:Pen) start length angle = 
        let x,y = start
        let endX, endY = endPointFrom length (degreeToRadian angle) (float32(x)) (float32(y))
        e.Graphics.DrawLine(p,x,y,endX,endY)

    let DrawTree (e : PaintEventArgs) t = 
        let maxWidthOfTree = 16.0*(2.0**float((treeHeight t - 1)))
        let initialY = 50.0
        let initialX = 50.0 + maxWidthOfTree*2.0
        let leafPen = new Pen(Color.Blue)
        let nodePen = new Pen(Color.Black)
        let initialCenter = initialX,initialY
        let radius = 10.0

        let drawLeafAt center label = DrawLeaf e leafPen center radius label
        let drawNodeAt center label = DrawLeaf e nodePen center radius label

        let rec loop n point depth k =
            match n with
            | Node(x,l,r) -> drawNodeAt point (string(x)) 
                             let currentX, _ = point
                             let currentShift = maxWidthOfTree/(float(depth))
                             loop l (currentX - currentShift, float(depth+1)*50.0) (depth+1) (fun lacc -> 
                             loop r (currentX + currentShift, float(depth+1)*50.0) (depth+1) (fun racc -> 
                             () |> k))
            | Leaf(x) -> drawLeafAt point (string(x)) |> k
        loop t initialCenter 1 id

    
        
    let DrawModel = 
        let form = new Form()
        form.Size <- new Size(800,800)

        let tree = Node(1, Node(2, Node(3, Leaf(4), Leaf(5)), Node(6,Leaf(7), Leaf(8))), Node(9, Node(10, Leaf(11), Leaf(12)), Node(13,Leaf(14), Leaf(15))))

        form.Paint.Add (fun e -> DrawTree e tree)
        Application.Run form


