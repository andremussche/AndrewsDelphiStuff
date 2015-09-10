{***********************************************************************
Denomo 2.1.0
http://www.kbasm.com/

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in
compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
for the specific language governing rights and limitations under the
License.

The Original Code is DenomoALAVLBinaryTree.pas

The Initial Developer of the Original Code is Wang Qi.
***********************************************************************}


// Please note, this is a modified version to used for Denomo.
// All copy right is to the original author, not to the Denomo author.

{*************************************************************
Author:       Stéphane Vander Clock (SVanderClock@Arkadia.com)
www:          http://www.arkadia.com
EMail:        SVanderClock@Arkadia.com

product:      TALAVLBinaryTree
Version:      3.10

Description:  - These binary trees are self-balancing in the AVL sense
                (the depth of any left branch differs by no more than
                one from the depth of the right branch).

              - Duplicate data is not allowed in a tree.

              - Nodes can be of type TALBaseAVLBinaryTreeNode or any
                descendant.

              - Next and Prev should not be used to iterate through an
                entire tree. This is much slower than calling the Iterate
                method.


Legal issues: Copyright (C) 1999-2005 by Arkadia Software Engineering

              This software is provided 'as-is', without any express
              or implied warranty.  In no event will the author be
              held liable for any  damages arising from the use of
              this software.

              Permission is granted to anyone to use this software
              for any purpose, including commercial applications,
              and to alter it and redistribute it freely, subject
              to the following restrictions:

              1. The origin of this software must not be
                 misrepresented, you must not claim that you wrote
                 the original software. If you use this software in
                 a product, an acknowledgment in the product
                 documentation would be appreciated but is not
                 required.

              2. Altered source versions must be plainly marked as
                 such, and must not be misrepresented as being the
                 original software.

              3. This notice may not be removed or altered from any
                 source distribution.

              4. You must register this software by sending a picture
                 postcard to the author. Use a nice stamp and mention
                 your name, street address, EMail address and any
                 comment you like to say.

Know bug :

History :     27/10/2005: Rebuild of the unit
              17/11/2005: improuve the nodecount property
              17/12/2005: add savetostream and loadfromstream
              10/01/2006: rebuild the unit to add AVL support
                          rename unit from ALBinaryTree to
                          ALAVLBinaryTree;
              25/02/2006: Update TALCardinalKeySessionAVLBinaryTree.CreateSessionNode
                          to delete expired node; 

Link :

Please send all your feedback to SVanderClock@Arkadia.com
**************************************************************}
unit DenomoALAVLBinaryTree;

interface
{
uses Classes,
     SysUtils,
     SyncObjs;
}
type
  {class defintion----------------}
  TALBaseAVLBinaryTreeNode = class;
  TALBaseAVLBinaryTree = class;

  {iterate function----------------------}
  TALAVLBinaryTreeIterateFunc = procedure(
                                          aTree: TALBaseAVLBinaryTree;
                                          aNode: TALBaseAVLBinaryTreeNode;
                                          aExtData: Pointer;
                                          Var aContinue: Boolean
                                         );



  {TALBaseAVLBinaryTreeNode---------------}
  TALBaseAVLBinaryTreeNode = class(Tobject)
  Private
  Protected
    ChildNodes: array[Boolean] of TALBaseAVLBinaryTreeNode;
    Bal: -1..1;
  Public
    Constructor Create; virtual;
  end;

  {TALBaseAVLBinaryTree---------------}
  TALBaseAVLBinaryTree = class(TObject)
  private
    FHead: TALBaseAVLBinaryTreeNode;
    FNodeCount: Integer;
  protected
    procedure FreeNodeObj(aNode: TALBaseAVLBinaryTreeNode); virtual;
    Function  CompareNode(IdVal: pointer; ANode: TALBaseAVLBinaryTreeNode): Integer; overload; Virtual; Abstract; {compares IdVal and aNode.keydata and returns 0 if they are equal. If IdVal is greater than aNode.keydata, returns an integer greater than 0. If aNode.keydata is less than IdVal, returns an integer less than 0.}
    Function  CompareNode(aNode1, ANode2: TALBaseAVLBinaryTreeNode): Integer; overload; Virtual; Abstract; {compares aNode1 and aNode2 and returns 0 if they are equal. If aNode1 is greater than aNode2, returns an integer greater than 0. If aNode1 is less than aNode2, returns an integer less than 0.}
    function  CreateNode: TALBaseAVLBinaryTreeNode; virtual; abstract;
    procedure InternalIterate(Action: TALAVLBinaryTreeIterateFunc; Up: Boolean; ExtData: Pointer); virtual;
    function  InternalAddNode(aNode: TALBaseAVLBinaryTreeNode): Boolean; virtual;
    Function  InternalDeleteNode(IdVal: Pointer): Boolean; virtual;
    Procedure InternalClear; Virtual;
    Function  InternalGetHead: TALBaseAVLBinaryTreeNode; virtual;
    function  InternalFindNode(idVal: pointer): TALBaseAVLBinaryTreeNode; virtual;
    function  InternalFirst: TALBaseAVLBinaryTreeNode; virtual; {Return the smallest-value node in the tree}
    function  InternalLast: TALBaseAVLBinaryTreeNode; virtual; {Return the largest-value node in the tree}
    function  InternalNext(aNode: TALBaseAVLBinaryTreeNode): TALBaseAVLBinaryTreeNode; virtual; {Return the next node whose value is larger than aNode}
    function  InternalPrev(aNode: TALBaseAVLBinaryTreeNode): TALBaseAVLBinaryTreeNode; virtual; {Return the largest node whose value is smaller than aNode}
    Function  InternalGetNodeCount: integer; virtual;
  public
    Constructor Create; virtual;
    Destructor  Destroy; Override;
    procedure   Iterate(Action: TALAVLBinaryTreeIterateFunc; Up: Boolean; ExtData: Pointer); virtual;
    function    AddNode(aNode: TALBaseAVLBinaryTreeNode): Boolean; virtual;
    Function    DeleteNode(IdVal: Pointer): Boolean; virtual;
    Procedure   Clear; Virtual;
    Function    Head: TALBaseAVLBinaryTreeNode; virtual;
    function    FindNode(idVal: pointer): TALBaseAVLBinaryTreeNode; virtual;
    function    First: TALBaseAVLBinaryTreeNode; virtual; {Return the smallest-value node in the tree}
    function    Last: TALBaseAVLBinaryTreeNode; virtual; {Return the largest-value node in the tree}
    function    Next(aNode: TALBaseAVLBinaryTreeNode): TALBaseAVLBinaryTreeNode; virtual; {Return the next node whose value is larger than aNode}
    function    Prev(aNode: TALBaseAVLBinaryTreeNode): TALBaseAVLBinaryTreeNode; virtual; {Return the largest node whose value is smaller than aNode}
    Function    NodeCount: integer; virtual;
  end;

implementation

//uses Contnrs;

{Following stack declarations are used to avoid recursion in all tree
 routines. Because the tree is AVL-balanced, a stack size of 40
 allows at least 2**32 elements in the tree without overflowing the
 stack.}

const
  cALAVLBinaryTree_StackSize = 40;
  cALAVLBinaryTree_LeftChild = False;
  cALAVLBinaryTree_RightChild = True;

type
  TALAVLBinaryTree_StackNode = record
    Node : TALBaseAVLBinaryTreeNode;
    Comparison : Integer;
  end;
  TALAVLBinaryTree_StackArray = array[1..cALAVLBinaryTree_StackSize] of TALAVLBinaryTree_StackNode;


{*************************************************}
function AlAVLBinaryTree_Sign(I: Integer): Integer;
begin
  if I < 0 then Result := -1
  else if I > 0 then Result := +1
  else Result := 0;
end;

{***********************************************************************}
procedure AlAVLBinaryTree_DelBalance(var aNode: TALBaseAVLBinaryTreeNode;
                                     var SubTreeDec: Boolean;
                                     CmpRes: Integer);
var N1, N2: TALBaseAVLBinaryTreeNode;
    B1, B2: Integer;
    LR: Boolean;
begin
  CmpRes := AlAVLBinaryTree_Sign(CmpRes);
  if aNode.Bal = CmpRes then aNode.Bal := 0
  else if aNode.Bal = 0 then begin
    aNode.Bal := -CmpRes;
    SubTreeDec := False;
  end
  else begin
    LR := (CmpRes < 0);
    N1 := aNode.ChildNodes[LR];
    B1 := N1.Bal;
    if (B1 = 0) or (B1 = -CmpRes) then begin
      {Single RR or LL rotation}
      aNode.ChildNodes[LR] := N1.ChildNodes[not LR];
      N1.ChildNodes[not LR] := aNode;
      if B1 = 0 then begin
        aNode.Bal := -CmpRes;
        N1.Bal := CmpRes;
        SubTreeDec := False;
      end
      else begin
        aNode.Bal := 0;
        N1.Bal := 0;
      end;
      aNode := N1;
    end
    else begin
      {Double RL or LR rotation}
      N2 := N1.ChildNodes[not LR];
      B2 := N2.Bal;
      N1.ChildNodes[not LR] := N2.ChildNodes[LR];
      N2.ChildNodes[LR] := N1;
      aNode.ChildNodes[LR] := N2.ChildNodes[not LR];
      N2.ChildNodes[not LR] := aNode;
      if B2 = -CmpRes then aNode.Bal := CmpRes
      else aNode.Bal := 0;
      if B2 = CmpRes then N1.Bal := -CmpRes
      else N1.Bal := 0;
      aNode := N2;
      N2.Bal := 0;
    end;
  end;
end;

{***********************************************************************}
procedure AlAVLBinaryTree_InsBalance(var aNode: TALBaseAVLBinaryTreeNode;
                                     var SubTreeInc: Boolean;
                                     CmpRes: Integer);
var N1: TALBaseAVLBinaryTreeNode;
    N2: TALBaseAVLBinaryTreeNode;
    LR: Boolean;
begin
  CmpRes := AlAVLBinaryTree_Sign(CmpRes);
  if aNode.Bal = -CmpRes then begin
    aNode.Bal := 0;
    SubTreeInc := False;
  end
  else if aNode.Bal = 0 then aNode.Bal := CmpRes
  else begin
    LR := (CmpRes > 0);
    N1 := aNode.ChildNodes[LR];
    if N1.Bal = CmpRes then begin
      aNode.ChildNodes[LR] := N1.ChildNodes[not LR];
      N1.ChildNodes[not LR] := aNode;
      aNode.Bal := 0;
      aNode := N1;
    end
    else begin
      N2 := N1.ChildNodes[not LR];
      N1.ChildNodes[not LR] := N2.ChildNodes[LR];
      N2.ChildNodes[LR] := N1;
      aNode.ChildNodes[LR] := N2.ChildNodes[not LR];
      N2.ChildNodes[not LR] := aNode;
      if N2.Bal = CmpRes then aNode.Bal := -CmpRes
      else aNode.Bal := 0;
      if N2.Bal = -CmpRes then N1.Bal := CmpRes
      else N1.Bal := 0;
      aNode := N2;
    end;
    aNode.Bal := 0;
    SubTreeInc := False;
  end;
end;

{***************************************************************************}
procedure AlAVLBinaryTree_IterateDestroyNodeFunc(aTree: TALBaseAVLBinaryTree;
                                                 aNode: TALBaseAVLBinaryTreeNode;
                                                 aExtData: Pointer;
                                                 Var aContinue: Boolean);

begin
  aTree.FreeNodeObj(aNode);
  acontinue := True;
end;




//////////////////////////////////////////////
////////// TALBaseAVLBinaryTreeNode //////////
//////////////////////////////////////////////

{******************************************}
constructor TALBaseAVLBinaryTreeNode.Create;
begin
 ChildNodes[cALAVLBinaryTree_LeftChild] := nil;
 ChildNodes[cALAVLBinaryTree_RightChild] := nil;
 Bal := 0;
end;




///////////////////////////////////////
////////// TALBaseAVLBinaryTree //////////
///////////////////////////////////////

{**************************************}
Constructor TALBaseAVLBinaryTree.Create;
begin
  FHead := Nil;
  FNodeCount := 0;
  Inherited;
end;

{**************************************}
Destructor TALBaseAVLBinaryTree.Destroy;
begin
  InternalClear;
  Inherited;
end;

{*********************************************************************************}
procedure TALBaseAVLBinaryTree.InternalIterate(Action: TALAVLBinaryTreeIterateFunc;
                                               Up: Boolean;
                                               ExtData: Pointer);
var N1: TALBaseAVLBinaryTreeNode;
    N2: TALBaseAVLBinaryTreeNode;
    StackPos: Integer;
    Stack: TALAVLBinaryTree_StackArray;
    Continue: Boolean;
begin
  Continue := True;
  StackPos := 0;
  N1 := Fhead;
  repeat
    while Assigned(N1) do begin
      Inc(StackPos);
      Stack[StackPos].Node := N1;
      N1 := N1.ChildNodes[not Up];
    end;
    if StackPos = 0 then Exit;

    N1 := Stack[StackPos].Node;
    Dec(StackPos);
    N2 := N1;
    N1 := N1.ChildNodes[Up];

    Action(Self, N2, ExtData, Continue);
    if not continue then Exit;
  until False;
end;


{**************************************************************************************}
function TALBaseAVLBinaryTree.InternalAddNode(aNode: TALBaseAVLBinaryTreeNode): Boolean;
var N1: TALBaseAVLBinaryTreeNode;
    CmpRes: Integer;
    StackPos: Integer;
    Stack: TALAVLBinaryTree_StackArray;
    SubTreeInc: Boolean;
begin
  {exit if node is nil}
  if not Assigned(aNode) then begin
    Result := False;
    Exit;
  end;

  {Handle first node}
  N1 := FHead;
  if not Assigned(N1) then begin
    Fhead := aNode;
    Inc(FNodeCount);
    result := True;
    Exit;
  end;

  {Find where new node should fit in tree}
  StackPos := 0;
  CmpRes := 0;
  while Assigned(N1) do begin

    {compare node}
    CmpRes := CompareNode(aNode, N1);

    {node already exist, so exit}
    if CmpRes = 0 then begin
      Result := False;
      Exit;
    end;

    {Build the stack}
    Inc(StackPos);
    with Stack[StackPos] do begin
      Node := N1;
      Comparison := CmpRes;
    end;

    {continue the loop}
    N1 := N1.ChildNodes[CmpRes > 0];

  end;

  {Insert new node}
  Stack[StackPos].Node.ChildNodes[CmpRes > 0] := aNode;
  Inc(FNodeCount);
  result := True;

  {Unwind the stack and rebalance}
  SubTreeInc := True;
  while (StackPos > 0) and SubTreeInc do begin
    if StackPos = 1 then ALAVLBinaryTree_InsBalance(Fhead, SubTreeInc, Stack[1].Comparison)
    else with Stack[StackPos-1] do
      AlAVLBinaryTree_InsBalance(Node.ChildNodes[Comparison > 0], SubTreeInc, Stack[StackPos].Comparison);
    dec(StackPos);
  end;
end;

{*******************************************}
procedure TALBaseAVLBinaryTree.InternalClear;
begin
  InternalIterate(
                  AlAVLBinaryTree_IterateDestroyNodeFunc,
                  True,
                  nil
                 );
  FHead := nil;
  FNodeCount := 0;
end;

{**********************************************************************}
function TALBaseAVLBinaryTree.InternalGetHead: TALBaseAVLBinaryTreeNode;
begin
  Result := Fhead;
end;

{**************************************************************************}
procedure TALBaseAVLBinaryTree.FreeNodeObj(aNode: TALBaseAVLBinaryTreeNode);
begin
  aNode.Free;
end;

{************************************************************************}
function TALBaseAVLBinaryTree.InternalDeleteNode(IdVal: Pointer): Boolean;
var N1: TALBaseAVLBinaryTreeNode;
    N2: TALBaseAVLBinaryTreeNode;
    TmpNode: TALBaseAVLBinaryTreeNode;
    CmpRes: Integer;
    Found: Boolean;
    SubTreeDec: Boolean;
    StackPos: Integer;
    StackParentPos: integer;
    Stack: TALAVLBinaryTree_StackArray;
begin
  {exit if head is nil}
  N1 := Fhead;
  if not Assigned(N1) then begin
    result := False;
    Exit;
  end;

  {Find node to delete and stack the nodes to reach it}
  Found := False;
  StackPos := 0;
  while not Found do begin

    {compare node}
    CmpRes := CompareNode(IdVal, N1);
    Inc(StackPos);

    {Found node}
    if CmpRes = 0 then begin
      with Stack[StackPos] do begin
        Node := N1;
        Comparison := -1;
      end;
      Found := True;
    end

    {not found yet, continue the search}
    else begin
      with Stack[StackPos] do begin
        Node := N1;
        Comparison := CmpRes;
      end;
      N1 := N1.ChildNodes[CmpRes > 0];

      {Node not found, then exit}
      if not Assigned(N1) then begin
        Result := False;
        Exit;
      end;
    end;

  end;

  {save the position of the parent of the node to delete in the stack}
  StackParentPos := StackPos - 1;

  {Delete the node found}
  N2 := N1;
  if (not Assigned(N2.ChildNodes[cALAVLBinaryTree_RightChild])) or (not Assigned(N2.ChildNodes[cALAVLBinaryTree_LeftChild])) then begin
    {Node has at most one branch}
    Dec(StackPos);
    N1 := N2.ChildNodes[Assigned(N2.ChildNodes[cALAVLBinaryTree_RightChild])];
    if StackPos = 0 then Fhead := N1
    else with Stack[StackPos] do
      Node.ChildNodes[Comparison > 0] := N1;
  end
  else begin
    {Node has two branches; stack nodes to reach one with no right child}
    N1 := N2.ChildNodes[cALAVLBinaryTree_LeftChild];
    while Assigned(N1.ChildNodes[cALAVLBinaryTree_RightChild]) do begin
      Inc(StackPos);
      with Stack[StackPos] do begin
        Node := N1;
        Comparison := 1;
      end;
      N1 := N1.ChildNodes[cALAVLBinaryTree_RightChild];
    end;

    {Swap the node to delete with the terminal node}
    N1.Bal := N2.Bal;
    If StackParentPos = 0 then Fhead := N1
    else with Stack[StackParentPos] do
      Node.ChildNodes[Comparison > 0] := N1;

    with Stack[StackParentPos+1] do
      Node := N1;

    tmpnode := N1.ChildNodes[cALAVLBinaryTree_LeftChild];
    N1.ChildNodes[cALAVLBinaryTree_RightChild] := N2.ChildNodes[cALAVLBinaryTree_RightChild];
    N1.ChildNodes[cALAVLBinaryTree_LeftChild] := N2.ChildNodes[cALAVLBinaryTree_LeftChild];

    with Stack[StackPos] do
      Node.ChildNodes[Comparison > 0] := tmpnode;
  end;

  {Dispose of the deleted node}
  FreeNodeObj(N2);
  Dec(FNodeCount);
  Result := True;

  {Unwind the stack and rebalance}
  SubTreeDec := True;
  while (StackPos > 0) and SubTreeDec do begin
    if StackPos = 1 then AlAVLBinaryTree_DelBalance(Fhead, SubTreeDec, Stack[1].Comparison)
    else with Stack[StackPos-1] do
      AlAVLBinaryTree_DelBalance(Node.ChildNodes[Comparison > 0], SubTreeDec, Stack[StackPos].Comparison);
    dec(StackPos);
  end;
end;

{***************************************************************************************}
function TALBaseAVLBinaryTree.InternalFindNode(idVal: pointer): TALBaseAVLBinaryTreeNode;
var N1: TALBaseAVLBinaryTreeNode;
    CmpRes: Integer;
begin
  N1 := FHead;
  while Assigned(N1) do begin
    CmpRes := CompareNode(IdVal, N1);
    if CmpRes = 0 then begin
      Result := N1;
      Exit;
    end
    else N1 := N1.ChildNodes[CmpRes > 0];
  end;

  Result := nil;
end;

{********************************************************************}
function TALBaseAVLBinaryTree.InternalFirst: TALBaseAVLBinaryTreeNode;
begin
  if FNodeCount = 0 then Result := nil
  else begin
    Result := Fhead;
    while Assigned(Result.ChildNodes[cALAVLBinaryTree_LeftChild]) do
      Result := Result.ChildNodes[cALAVLBinaryTree_LeftChild];
  end;
end;

{*******************************************************************}
function TALBaseAVLBinaryTree.InternalLast: TALBaseAVLBinaryTreeNode;
begin
  if FNodeCount = 0 then Result := nil
  else begin
    Result := FHead;
    while Assigned(Result.ChildNodes[cALAVLBinaryTree_RightChild]) do
      Result := Result.ChildNodes[cALAVLBinaryTree_RightChild];
  end;
end;

{****************************************************************************************************}
function TALBaseAVLBinaryTree.InternalNext(aNode: TALBaseAVLBinaryTreeNode): TALBaseAVLBinaryTreeNode;
var Found: Word;
    N1: TALBaseAVLBinaryTreeNode;
    StackPos: Integer;
    Stack: TALAVLBinaryTree_StackArray;
begin
  Result := nil;
  Found := 0;
  StackPos := 0;
  N1 := FHead;
  repeat
    while Assigned(N1) do begin
      Inc(StackPos);
      Stack[StackPos].Node := N1;
      N1 := N1.ChildNodes[cALAVLBinaryTree_LeftChild];
    end;
    if StackPos = 0 then Exit;

    N1 := Stack[StackPos].Node;
    Dec(StackPos);
    if Found = 1 then begin
      Result := N1;
      Exit;
    end;
    if N1 = aNode then Inc(Found);
    N1 := N1.ChildNodes[cALAVLBinaryTree_RightChild];
  until False;
end;

{****************************************************************************************************}
function TALBaseAVLBinaryTree.InternalPrev(aNode: TALBaseAVLBinaryTreeNode): TALBaseAVLBinaryTreeNode;
var Found: Word;
    N1: TALBaseAVLBinaryTreeNode;
    StackPos: Integer;
    Stack: TALAVLBinaryTree_StackArray;
begin
  Result := nil;
  Found := 0;
  StackPos := 0;
  N1 := FHead;
  repeat
    while Assigned(N1) do begin
      Inc(StackPos);
      Stack[StackPos].Node := N1;
      N1 := N1.ChildNodes[cALAVLBinaryTree_RightChild];
    end;
    if StackPos = 0 then Exit;

    N1 := Stack[StackPos].Node;
    Dec(StackPos);
    if Found = 1 then begin
      Result := N1;
      Exit;
    end;
    if N1 = aNode then
      Inc(Found);
    N1 := N1.ChildNodes[cALAVLBinaryTree_LeftChild];
  until False;
end;

{**********************************************************}
function TALBaseAVLBinaryTree.InternalGetNodeCount: integer;
begin
  Result := FnodeCount;
end;

{******************************************************************************}
function TALBaseAVLBinaryTree.AddNode(aNode: TALBaseAVLBinaryTreeNode): Boolean;
begin
  Result := InternalAddNode(aNode);
end;

{***********************************}
procedure TALBaseAVLBinaryTree.Clear;
begin
  InternalClear;
end;

{****************************************************************}
function TALBaseAVLBinaryTree.DeleteNode(IdVal: Pointer): Boolean;
begin
  Result := InternalDeleteNode(IdVal);
end;

{*******************************************************************************}
function TALBaseAVLBinaryTree.FindNode(idVal: pointer): TALBaseAVLBinaryTreeNode;
begin
  Result := InternalFindNode(idVal);
end;

{************************************************************}
function TALBaseAVLBinaryTree.First: TALBaseAVLBinaryTreeNode;
begin
  result := InternalFirst;
end;

{***********************************************************}
function TALBaseAVLBinaryTree.Head: TALBaseAVLBinaryTreeNode;
begin
  Result := InternalGetHead;
end;

{*********************************************************************************************************}
procedure TALBaseAVLBinaryTree.Iterate(Action: TALAVLBinaryTreeIterateFunc; Up: Boolean; ExtData: Pointer);
begin
  InternalIterate(Action, Up, ExtData);
end;

{***********************************************************}
function TALBaseAVLBinaryTree.Last: TALBaseAVLBinaryTreeNode;
begin
  Result := InternalLast;
end;

{********************************************************************************************}
function TALBaseAVLBinaryTree.Next(aNode: TALBaseAVLBinaryTreeNode): TALBaseAVLBinaryTreeNode;
begin
  Result := InternalNext(aNode);
end;

{********************************************************************************************}
function TALBaseAVLBinaryTree.Prev(aNode: TALBaseAVLBinaryTreeNode): TALBaseAVLBinaryTreeNode;
begin
  Result := InternalPrev(aNode);
end;

{***********************************************}
function TALBaseAVLBinaryTree.NodeCount: integer;
begin
  Result := InternalGetNodeCount;
end;

end.
