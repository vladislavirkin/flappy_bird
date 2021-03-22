unit main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Ani,
  FMX.Objects, FMX.Layouts, FMX.StdCtrls, FMX.Controls.Presentation;

type
  TForm1 = class(TForm)
    LayoutWorld: TLayout;
    LayoutBird: TLayout;
    Ellipse1: TEllipse;
    Ellipse2: TEllipse;
    Circle1: TCircle;
    Circle2: TCircle;
    PieWing: TPie;
    FloatAnimationWing: TFloatAnimation;
    LayoutObstacle: TLayout;
    Rectangle1: TRectangle;
    FloatAnimationRotating: TFloatAnimation;
    FloatAnimationFalling: TFloatAnimation;
    LayoutMessage: TLayout;
    lbPointsMax: TLabel;
    lbPoints: TLabel;
    btStart: TButton;
    RectangleHint: TRectangle;
    ColorAnimation1: TColorAnimation;
    Rectangle2: TRectangle;
    procedure FloatAnimationWingProcess(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure LayoutWorldMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure FloatAnimationFallingFinish(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure LayoutBirdPainting(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
  private
    { Private declarations }
  public
    procedure createWorld;
    procedure Reset;
    procedure WayMaker(const aMaster: TLayout);
    procedure GameOver;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

const
  maxDistance: single = 230;
  maxCountObstacles: Integer = 8;

procedure TForm1.WayMaker(const aMaster: TLayout);
var
  h, m: TPointF;
  r, e: Single;
begin
  h.X := LayoutWorld.Height;
  h.Y := 2;
  m.X := LayoutBird.Height * 6;

  e := m.X / h.X;

  repeat
    r := Random;
  until ((TagFloat + e) > r) and ((TagFloat - e) < r);

  TagFloat := r;
  m.Y := r * (h.X -m.X - 2 * h.Y);

  with TRectangle(aMaster.Children[0]) do
    Height := h.Y + m.Y;
  with TRectangle(aMaster.Children[1]) do
    Height := h.X - h.Y - m.Y - m.X;

  aMaster.Tag := 0;
end;

procedure TForm1.createWorld;
var
  p: Single;
  I: Integer;
  c: TLayout;
begin
  LayoutWorld.HitTest := True;
  LayoutWorld.Tag := 0;
  p := LayoutWorld.Width;
  LayoutObstacle.Position.X := p;
  for I := 2 to maxCountObstacles do
  begin
    c := TLayout(LayoutObstacle.Clone(LayoutWorld));
    p := p + LayoutObstacle.Width + maxDistance;
    c.Position.X := p;
    LayoutWorld.AddObject(c);
  end;

  TRectangle(c.Children[0]).Fill.Color := TAlphaColors.Orange;
  TRectangle(c.Children[1]).Fill.Color := TAlphaColors.Orange;

  btStart.Visible := False;
  Reset;
end;

procedure TForm1.Reset;
var
  p: Single;
  I: Integer;
  c: TLayout;
  t: TPointF;
begin
  RandSeed := 0; //342
  TagFloat := Random;
  p := LayoutWorld.Width;
  LayoutObstacle.Position.X := p;

  for I := 0 to LayoutWorld.ChildrenCount - 1 do
  begin
    if not (LayoutWorld.Children[I] is TLayout) then
      Continue;

    c := TLayout(LayoutWorld.Children[I]);
    if c = LayoutBird then
      Continue;

    WayMaker(c);

    if c = LayoutObstacle then
      Continue;

    p := p + LayoutObstacle.Width + maxDistance;
    c.Position.X := p;
  end;

  with LayoutWorld do
  begin
    t := PointF(Width, Height);
  end;

  with LayoutBird do
  begin
    Position.Point := (t - PointF(Width, Height)) * 0.5;
  end;

  LayoutBird.RotationAngle := 0;
  PieWing.EndAngle := -90;
  LayoutBird.BringToFront;

  LayoutWorld.HitTest := True;

  btStart.Tag := 0;
  lbPointsMax.Text := IntToStr(LayoutWorld.Tag);
  lbPoints.Text := IntToStr(btStart.Tag);
end;

procedure TForm1.GameOver;
begin
  LayoutWorld.HitTest := False;
  btStart.Text := 'Game over' + LineFeed
                + 'Points: ' + IntToStr(btStart.Tag) + LineFeed
                + 'Best: ' + IntToStr(LayoutWorld.Tag);
  btStart.Visible := True;
  btStart.BringToFront;

  if (LayoutWorld.Tag < btStart.Tag) then
    LayoutWorld.Tag := btStart.Tag;
end;

procedure TForm1.btStartClick(Sender: TObject);
begin
  btStart.Visible := False;
  Reset;
end;

procedure TForm1.FloatAnimationFallingFinish(Sender: TObject);
begin
  FloatAnimationRotating.Start;
end;

procedure TForm1.FloatAnimationWingProcess(Sender: TObject);
var
  I: Integer;
  c: TLayout;
  r: TRectF;
  k: Integer;
begin
  with LayoutBird do
  begin
    if Position.X <> (LayoutWorld.Width - Width) * 0.5 then
      Position.X := (LayoutWorld.Width - Width) * 0.5;
    if LayoutWorld.AbsoluteRect.Bottom > AbsoluteRect.Bottom then
      Position.Y := Position.Y + RotationAngle * 0.1
    else
    begin
      FloatAnimationWing.StopAtCurrent;
      GameOver;
      Exit;
    end;

    r := AbsoluteRect;
  end;

  if not LayoutWorld.HitTest then
    Exit;

  for I := 0 to LayoutWorld.ChildrenCount - 1 do
  begin
    if not (LayoutWorld.Children[i] is TLayout) then
      Continue;

    c := TLayout(LayoutWorld.Children[I]);

    if c = LayoutBird then
      Continue;

    if c.AbsoluteRect.IntersectsWith(r) and (c.Tag = 0) then
    begin
      c.Tag := 1;
      btStart.Tag := btStart.Tag + 1;
      lbPoints.Text := IntToStr(btStart.Tag);
    end;

    if TRectangle(c.Children[0]).AbsoluteRect.IntersectsWith(r) or
      TRectangle(c.Children[1]).AbsoluteRect.IntersectsWith(r) then
      begin
        for k := 0 to LayoutBird.ChildrenCount - 1 do
        begin
          if LayoutBird.Children[k] is TControl then
          begin
            r := TLayout(LayoutBird.Children[k]).AbsoluteRect;

            if TRectangle(c.Children[0]).AbsoluteRect.IntersectsWith(r) or
              TRectangle(c.Children[1]).AbsoluteRect.IntersectsWith(r) then
            begin
              LayoutWorld.HitTest := False;
              ColorAnimation1.Start;
              Exit;
            end;
          end;
        end;
      end;


    if c.Position.X + LayoutObstacle.Width < 0 then
    begin
      c.Position.X := maxCountObstacles * (LayoutObstacle.Width + maxDistance) -
                   LayoutBird.Width;

      WayMaker(c);
    end
    else
    begin
      c.Position.X := c.Position.X - 3;
    end;
  end;
end;

procedure TForm1.LayoutBirdPainting(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin
//  Canvas.ClearRect(ARect, TAlphaColors.Yellow);
end;

procedure TForm1.LayoutWorldMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if not FloatAnimationWing.Running then
    FloatAnimationWing.Start;

  FloatAnimationRotating.StopAtCurrent;
  LayoutBird.RotationAngle := -10;
  FloatAnimationFalling.StopValue := LayoutBird.Position.Y -
    LayoutBird.Height * 3;

  if FloatAnimationFalling.StopValue < LayoutBird.Height then
    FloatAnimationFalling.StopValue := LayoutBird.Height;

  FloatAnimationFalling.Start;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  if Key = 27 then
    Close;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  createWorld;
end;
end.
