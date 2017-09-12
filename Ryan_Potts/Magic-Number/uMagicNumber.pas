unit uMagicNumber;

interface

  function MagicNumber(aInputValues: array of double; out aMagicNumber: double): TArray<TArray<double>>;

implementation

function MagicNumber(aInputValues: array of double; out aMagicNumber: double): TArray<TArray<double>>;
var lResult: TArray<TArray<double>>;

  function findMagicNumber: Double;
  var Accum: double;
      Value: double;
  begin
    Accum := 0;
    for Value in aInputValues do
      Accum := Accum + Value;
    result := Accum / 3;
  end;

  procedure fillCentralMatrixValue;
  var
     midIndex: integer;
  begin
    midIndex := length(aInputValues) div 2;
    lResult[1,1] := aInputValues[midIndex];
  end;

  procedure fillCornerMatrixValues;
  var
    lCentralVal: double;
  begin
    lCentralVal := lResult[1,1];

    //Upper left
    lResult[0,0] := lCentralVal + 0.5;
    //Upper right
    lResult[0,2] := lCentralVal + 1.5;
    //Lower left
    lResult[2,0] := lCentralVal - 1.5;
    //Lower right
    lResult[2,2] := lCentralVal - 0.5;
  end;

  procedure fillMiddleMatrixValues;
  var
    lCentralVal: double;
  begin
    lCentralVal := lResult[1,1];

    //Top central
    lResult[0,1] := lCentralVal - 2.0;
    //Left central
    lResult[1,0] := lCentralVal + 1.0;
    //Bottom central
    lResult[2,1] := lCentralVal + 2.0;
    //Right central
    lResult[1,2] := lCentralVal - 1.0;
  end;

begin
  aMagicNumber := findMagicNumber;

  SetLength(lResult, 3, 3);
  fillCentralMatrixValue;
  fillCornerMatrixValues;
  fillMiddleMatrixValues;

  Result := lResult;
end;

end.
