unit uMagicNumberTest;

interface
uses
  DUnitX.TestFramework, Generics.Collections;

type

  [TestFixture]
  MagicNumberTest = class(TObject)
  private
    const
      FMagicNumber: double = 9.0;
    var
      FInputValues: TArray<double>;
      FExpectedFull: TArray<TArray<double>>;
    procedure compareDynamicArrays(Array1, Array2: TArray<TArray<double>>);
  public
    [Setup]
    procedure Setup;

    [Test]
    procedure Find_the_magic_number;

    [Test]
    procedure Find_3x3_Matrix;
  end;

implementation
uses SysUtils, uMagicNumber;

procedure MagicNumberTest.Setup;
var
  i: integer;
  lList: TList<double>;
  lListMatrix: TList<TList<double>>;
begin
  lList := TList<double>.Create;
  lList.AddRange([1,1.5,2,2.5,3,3.5,4,4.5,5]);
  FInputValues := lList.ToArray;
  lList.DisposeOf;

  lListMatrix := TList<Tlist<double>>.Create;
  lListMatrix.Add(TList<double>.Create);
  lListMatrix.Add(TList<double>.Create);
  lListMatrix.Add(TList<double>.Create);
  lListMatrix[0].AddRange([3.5, 1.0, 4.5]);
  lListMatrix[1].AddRange([4.0, 3.0, 2.0]);
  lListMatrix[2].AddRange([1.5, 5.0, 2.5]);
  SetLength(FExpectedFull, 3);
  FExpectedFull[0] := lListMatrix[0].ToArray;
  FExpectedFull[1] := lListMatrix[1].ToArray;
  FExpectedFull[2] := lListMatrix[2].ToArray;
  lListMatrix.DisposeOf;
end;

procedure MagicNumberTest.Find_the_magic_number;
var ActualMagicNumber: double;
begin
  MagicNumber(FInputValues, ActualMagicnumber);
  Assert.AreEqual(FMagicNumber, ActualMagicNumber);
end;


procedure MagicNumberTest.compareDynamicArrays(Array1,
  Array2: TArray<TArray<double>>);
var yLengthA1,
    yLengthA2,
    xLengthA1,
    xLengthA2: integer;
    x, y: integer;
begin
  //First test that both arrays are the same size
  yLengthA1 := Length(Array1);
  yLengthA2 := Length(Array2);
  Assert.AreEqual(yLengthA1, yLengthA2);

  xLengthA1 := Length(Array1[0]);
  xLengthA2 := Length(Array2[0]);
  Assert.AreEqual(xLengthA1, xLengthA2);

  //Test that all values are equal
  for y := Low(Array1) to High(Array1) do
    for x := Low(Array1[0]) to High(Array1[0]) do
      Assert.AreEqual(Array1[y,x], Array2[y,x],format('y=%d, x=%d ',[y,x]));
end;

procedure MagicNumberTest.Find_3x3_Matrix;
var ActualMagicNumber: double;
    ActualMatrixFull: TArray<TArray<double>>;
begin
  ActualMatrixFull := MagicNumber(FInputValues, ActualMagicnumber);
  compareDynamicArrays(FExpectedFull, ActualMatrixFull);
end;

initialization
  TDUnitX.RegisterTestFixture(MagicNumberTest);
end.
