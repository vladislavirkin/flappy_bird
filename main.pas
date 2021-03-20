unit main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Ani,
  FMX.Objects, FMX.Layouts;

type
  TForm1 = class(TForm)
    LayoutWorld: TLayout;
    LayoutBird: TLayout;
    Ellipse1: TEllipse;
    Ellipse2: TEllipse;
    Circle1: TCircle;
    Circle2: TCircle;
    Pie1: TPie;
    FloatAnimationWing: TFloatAnimation;
    LayoutObstacle: TLayout;
    Rectangle1: TRectangle;
    Rectangle2: TRectangle;
    procedure FloatAnimationWingProcess(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    procedure Reset;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

const
  maxObstacle: single = 200;

procedure TForm1.FloatAnimationWingProcess(Sender: TObject);
var
  I: Integer;
  c: TLayout;
begin
  with LayoutBird do
  begin
    if Position.X <> (LayoutWorld.Width - Width) * 0.5 then
      Position.X := (LayoutWorld.Width - Width) * 0.5;
    if LayoutWorld.AbsoluteRect.Bottom <> AbsoluteRect.Bottom then
      Position.Y := Position.Y - RotationAngle * 0.1;
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

    if c.Position.X + LayoutObstacle.Width < 0 then
    begin
      
    end
    else
    begin
      c.Position.X := c.Position.X - 3;
    end;
  end;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  if Key = 27 then
    Close;
end;

procedure TForm1.Reset;
begin

end;

procedure TForm1.FormCreate(Sender: TObject);
var
  p: Single;
  I: Integer;
  c: TLayout;
begin
  LayoutWorld.HitTest := True;
  LayoutWorld.Tag := 0;
  p := LayoutWorld.Width;
  LayoutObstacle.Position.X := p;
  for I := 2 to 15 do
  begin
    c := TLayout(LayoutObstacle.Clone(LayoutWorld));
    p := p + LayoutObstacle.Width + maxObstacle;
    c.Position.X := p;
    LayoutWorld.AddObject(c);    
  end;  

  TRectangle(c.Children[0]).Fill.Color := TAlphaColors.Orange;
  TRectangle(c.Children[0]).Fill.Color := TAlphaColors.Orange;

  Reset;
end;

end.
