/**
 * phylo_tree - Parse a tree in Newick notation 
 *   and draw it in an HTML <div>.  The two parameters are 
 *   the text string of the Newick format and the text ID 
 *   of the <div> in which the tree is to be drawn.  If
 *   the <div> contains any child HTML elements, they will 
 *   be deleted by the "clear()" method of the tree 
 *   drawing object.
 * 
 * @author Christopher A. Meacham
 * Berkeley Phylogenomics Group
 * University of California, Berkeley
 * Copyright � 2008 Regents of the University of California
 * Derived from earlier work Copyright � 1984-2008 Christopher A. Meacham
 * 
 */

function writeTime(aTime, aStr) {       
//      var currentTime = new Date();
    var currentTime = aTime;
    var hours = currentTime.getHours();
    var minutes = currentTime.getMinutes();
    var seconds = currentTime.getSeconds();
    var timeStr = aStr + ": " + hours + ":" + minutes + ":" + seconds;  
    document.getElementById("righttreediv").innerHTML += "<br />" + timeStr + "<br />"; 
//      window.focus();
//      alert(timeStr);
}
  
function LineDiv(aX1, aY1, aX2, aY2, aLineWidth, aColor, aCanvasDiv) {
  var _halfLineWidth = Math.round(aLineWidth/2);
  this.lineDiv = top.window.document.createElement("div");
  aCanvasDiv.appendChild(this.lineDiv);
  this.lineDiv.style.position = "absolute";
  this.lineDiv.style.backgroundColor = aColor;
//      clip:rect(0,'+w+'px,'+h+'px,0); 
  this.placeLine = placeLineWZ;
  this.placeLine();

// This placeLineWZ method conforms to the placment of lines in Walter Zorn's JavaScript
//   graphics package   
  function placeLineWZ() {
    if (aY1==aY2) {
      var _w = Math.abs(aX2-aX1)+_halfLineWidth+1;
      this.lineDiv.style.top = Math.min(aY1,aY2)+"px";
      this.lineDiv.style.height = aLineWidth+"px";
      this.lineDiv.style.left = Math.min(aX1,aX2)+"px"; 
      this.lineDiv.style.width = _w+"px";
//                      this.lineDiv.style.clip = "rect(0,"+_w+"px,"+aLineWidth+"px, 0);"
    } else {
      var _h = Math.abs(aY2-aY1)+aLineWidth;
      this.lineDiv.style.top = Math.min(aY1,aY2)+"px";
      this.lineDiv.style.height = _h+"px";
      this.lineDiv.style.left = Math.min(aX1,aX2)+"px"; 
      this.lineDiv.style.width = aLineWidth+"px";
//                      this.lineDiv.style.clip = "rect(0,"+aLineWidth+"px,"+_h+"px, 0);"
    }
  }
  
}

function newickNodeLabelDiv(aNode, aCanvasDiv) {
  var _div = top.window.document.createElement("div");
  aCanvasDiv.appendChild(_div);
  _div.node = aNode;     // Add a node property to the <div> for use in events below
  _div.align = "center";
  _div.valign = "center";
  _div.style.position = "absolute";
//      _div.style.border = "1px solid black";
  _div.onmouseover = function() {
    this.node.onMouseOver();   // Here, "this" refers to the <div> element
    }
  _div.onmouseout = function() {
    this.node.onMouseOut();
    }
//      _div.onclick = function() {
//              this.node.onClick();    
//      }
  return (_div);
}


/**
 * 
 * newickNode object - a single node of a Newick tree
 * 
 */

function newickNode() {
// Properties of the abstract tree
  this.ancestor;
  this.descendant;
  this.sibling;
  this.lastDescendant;
  this.leftId;
  this.rightId;
  this.label = "";               // Label of this node as read from Newick description
  this.text = "";                // Text to display for this node in mouseover
  this.length = 0.0;             // Length of stem below this node as read from Newick description
  this.lengthFromRoot = 0.0;     // Sum of lengths from tree root to this node
  this.level = 0.0;
  this.levelLength = 0.0;
  this.order = 0.0;              // Numeric value of node position with respect to tips
                         // First tip is 0.0; last tip is (number of tips)-1
  this.subtendedTipCount = 0;    // Count of distal tips above this node
  this.nhx_isPresent = false;    // True if any "New Hampshire Extended" attributes are defined
  this.nhx_S = "";               // "New Hampshire Extended" format attributes for this node
  this.nhx_D = "";
  this.nhx_B = 0.0;
// Properties related to the graphical representation
  this.tree = null;
  this.labelDiv = null;
  this.buttonDiv = null;
  this.collapsedDiv = null;
  this.x = 0.0;
  this.y = 0.0;
  this.stemX = 0.0;
  this.stemY = 0.0;
  this.isCollapsed = false;
  this.isVisible = true;
  this.hasPointedAtDesc = false;
  this.isInSet = false;
  this.isHighlighted = false;
  this.isStemInsignificant = false; // True if a branch is drawn, but length is actually zero
  this.labelBackgroundColor = "";   
  this.lineDivStemColor = null;
  this.lineDivSpanColor = null;
  this.lineDivStem = null;
  this.lineDivSpan = null;
  this.lineDivSpanHighlight = null;
  this.linePointer = null;
  this.subfamily = "";
  
  this.createLabelDiv = function() {
    if (!this.labelDiv) {
      this.labelDiv = newickNodeLabelDiv(this, this.tree.canvasDiv);
      this.labelDiv.style.zIndex = this.tree.zIndexLabel;
      if (!this.tree.onLoadLabelDiv) {
        if (this.label != "") {
          this.labelDiv.innerHTML = this.label;
        }
      } else {
        this.tree.onLoadLabelDiv(this); 
      }
    }
//              this.labelDivWidth = (+this.labelDiv.offsetWidth);
//              this.labelDivHeight = this.labelDiv.style.height;
  }

  this.setLabelHTML = function(aStr) {
    if (!this.labelDiv) {
      this.createLabelDiv();
    }
    this.labelDiv.innerHTML = aStr;
  }
  
  this.createButtonDiv = function() {
    if (!this.buttonDiv) {
      this.buttonDiv = newickNodeLabelDiv(this, this.tree.canvasDiv);
      this.buttonDiv.style.zIndex = this.tree.zIndexLabel;
      this.buttonDiv.style.backgroundColor = "";
      this.buttonDiv.style.width = this.tree.labelButtonSize + "px";
      this.buttonDiv.style.height = this.tree.labelButtonSize + "px";
      this.buttonDiv.onmouseover = function() {
        this.node.onButtonMouseOver();   // Here, "this" refers to the <div> element
        }
      this.buttonDiv.onmouseout = function() {
        this.node.onButtonMouseOut();
        }
      this.buttonDiv.onclick = function() {
        this.node.onButtonClick();    
      }
    }
  } 
  
  this.positionTextDivs = function() {
    
    function spot(x, y, aCanvasDiv) {
      var _div = window.document.createElement("div");
      aCanvasDiv.appendChild(_div);
      _div.style.position = "absolute";
      _div.style.top = y;
      _div.style.left = x;
      _div.style.height = 1;
      _div.style.width = 1;
      _div.style.backgroundColor = "red";
      _div.style.zIndex = 25;
      return(_div);
    }
  
    if (!this.tree.isRootedOnRight) {
      if (this.labelDiv) {
//spot(this.x, this.y, this.tree.canvasDiv);
        this.labelDiv.style.left = (this.x + this.tree.treeLineWidth + this.tree.spacingTipToLabel) + "px";
        this.labelDiv.style.top = (this.y - Math.round((this.tree.labelHeight - this.tree.treeLineWidth) / 2) - 1) + "px";
      }
      if (this.buttonDiv) {
        this.buttonDiv.style.left = (this.x + this.tree.treeLineWidth - Math.round(this.tree.labelButtonSize / 2)
          - this.tree.spacingTipToLabel) + "px";
        this.buttonDiv.style.top = (this.y - Math.round((this.tree.labelButtonSize - this.tree.treeLineWidth) / 2)) + "px";
      }
      if (this.collapsedDiv) {
        this.collapsedDiv.style.left = (this.x + this.tree.treeLineWidth + this.tree.spacingTipToLabel) + "px";
        this.collapsedDiv.style.top = (this.y - Math.round((this.tree.labelHeight - this.tree.treeLineWidth) / 2) - 1) + "px";
      }
    } else {
      if (this.labelDiv) {
//spot(this.x, this.y, this.tree.canvasDiv);
        this.labelDiv.style.left = (this.x - this.labelDiv.offsetWidth - this.tree.spacingTipToLabel) + "px";
        this.labelDiv.style.top = (this.y - Math.round((this.tree.labelHeight - this.tree.treeLineWidth) / 2) - 1) + "px";
      }
      if (this.buttonDiv) {
        this.buttonDiv.style.left = (this.x - Math.round(this.tree.labelButtonSize / 2) 
          + this.tree.spacingTipToLabel) + "px";
        this.buttonDiv.style.top = (this.y - Math.round((this.tree.labelButtonSize - this.tree.treeLineWidth) / 2)) + "px";
      }
      if (this.collapsedDiv) { //Check left
        this.collapsedDiv.style.left = (this.x - this.collapsedDiv.offsetWidth - this.tree.spacingTipToLabel) + "px";
        this.collapsedDiv.style.top = (this.y - Math.round((this.tree.labelHeight - this.tree.treeLineWidth) / 2) - 1) + "px";
      }
    }
  }

  this.placeLabelDiv = function(aX, aY) {
    this.labelDiv.style.left = aX;
    this.labelDiv.style.top = aY;
  }

  this.createLabel = function(){
    this.createButtonDiv();
    if (!this.descendant) {
      this.createLabelDiv();
      this.labelDiv.style.backgroundColor = this.tree.labelBackgroundColor;                             
    } else {                    
      var labelText = "";
      if (this.tree.doShowGeneDuplicationEvents) {
        switch (this.nhx_D) {
          case "Y":
            labelText = "D";
            this.labelDiv.style.backgroundColor = this.tree.internalLabelNhxDYColor;
            this.labelBackgroundColor = this.tree.internalLabelNhxDYColor;
            break;
          case "N":
            labelText = "N";
            this.labelDiv.style.backgroundColor = this.tree.internalLabelNhxDNColor;
            this.labelBackgroundColor = this.tree.internalLabelNhxDNColor;
        }
      } else if (!this.tree.suppressInternalNodeLabels) {
        labelText = aNode.label;
      }
    }
  }
      
  this.drawLabel = function() {
    if (!this.labelDiv) {
      this.createLabel();
    }
    if (this.labelDiv) {
      if (this.isInSet) {
        this.labelDiv.style.backgroundColor = this.tree.labelSelectBackgroundColor;
      }
      else {
        this.labelDiv.style.backgroundColor = this.labelBackgroundColor;
      }
    }
  }

    this.drawLineDiv = function(aLineDiv, aX1, aY1, aX2, aY2, aLineWidth, aColor) {
    if (!aLineDiv.lineDiv) {
      aLineDiv.lineDiv = top.window.document.createElement("div");
      this.tree.canvasDiv.appendChild(aLineDiv.lineDiv);
      aLineDiv.lineDiv.style.position = "absolute";
      aLineDiv.lineDiv.normalColor = aColor;
      aLineDiv.lineDiv.highlightColor = this.tree.treeLineHighlightColor;
      aLineDiv.lineDiv.node = this;
    }   
    var _halfLineWidth = Math.round(aLineWidth/2);
    aLineDiv.lineDiv.style.backgroundColor = aColor;
    var _placeLine = placeLineWZ;
    _placeLine();
  
  // This PlaceLineWZ method conforms to the placment of lines in Walter Zorn's JavaScript
  //   graphics package 
    function placeLineWZ() {
      if (aY1==aY2) {
        var _w = Math.abs(aX2-aX1)+_halfLineWidth+1;
        aLineDiv.lineDiv.style.top = Math.min(aY1,aY2) + "px";
        aLineDiv.lineDiv.style.height = aLineWidth + "px";
        aLineDiv.lineDiv.style.left = Math.min(aX1,aX2) + "px"; 
        aLineDiv.lineDiv.style.width = _w + "px";
//                              aLineDiv.lineDiv.style.clip = "rect(0,"+_w+"px,"+aLineWidth+"px, 0);"
      } else {
        var _h = Math.abs(aY2-aY1)+aLineWidth;
        aLineDiv.lineDiv.style.top = Math.min(aY1,aY2) + "px";
        aLineDiv.lineDiv.style.height = _h + "px";
        aLineDiv.lineDiv.style.left = Math.min(aX1,aX2) + "px"; 
        aLineDiv.lineDiv.style.width = aLineWidth + "px";
//                              aLineDiv.lineDiv.style.clip = "rect(0,"+aLineWidth+"px,"+_h+"px, 0);"
      }
    }
  }
  
  this.drawPointer = function() {
    if (this.isPointedAt || (this.hasPointedAtDesc && this.isCollapsed && this.isVisible)) {
      if (!this.linePointer) {
        this.linePointer = new Object();
      }
      var _labelWidth = 0;
      if (this.isCollapsed) {
        if (this.collapsedDiv) {
          _labelWidth = this.collapsedDiv.offsetWidth;
        }
      } else {
        if (this.labelDiv) {
          _labelWidth = this.labelDiv.offsetWidth;
        }
      }
      if (!this.tree.isRootedOnRight) {
        this.drawLineDiv(this.linePointer, this.x + this.tree.treeLineWidth + _labelWidth +
          2 * this.tree.spacingTipToLabel, this.y, this.tree.selectX, this.y, this.tree.treeLineWidth, 
          this.tree.linePointerColor);
      } else {
        this.drawLineDiv(this.linePointer, this.tree.selectX, this.y, this.x - _labelWidth -
          2 * this.tree.spacingTipToLabel - this.tree.treeLineWidth, this.y, this.tree.treeLineWidth, 
          this.tree.linePointerColor);
      } 
    } 
    if (this.linePointer) {
      if (this.isVisible && !(this.descendant && this.descendant.isVisible)) {
        this.linePointer.lineDiv.style.visibility = "visible";
      } else {
        this.linePointer.lineDiv.style.visibility = "hidden";
      }                 
    }
  }
  
//  In the bodies of the following three event functions, "this" (at execution time) refers to the <div> element 
//    that has been assigned the event function, not to this newickNode object.  Each <div> has a "node" property 
//    that allows references to the properties of the node to which each <div> belongs during execution.

  this.lineDivSpanMouseOver = function() {
    this.style.backgroundColor = this.highlightColor;
    phylo_Tip("Level:&nbsp;"+this.node.lengthFromRoot);
  }
  
  this.lineDivSpanMouseOut = function() {
    this.style.backgroundColor = this.node.lineDivSpanColor;
    UnTip();
  }
  
  this.lineDivStemMouseOver = function() {
    this.style.backgroundColor = this.highlightColor;
    phylo_Tip("Length:&nbsp;"+this.node.length);
  }
  
  this.lineDivStemMouseOut = function() {
    if (this.node.isStemHighlighted()) {
      this.style.backgroundColor = this.node.tree.treeLineSelectColor;
    } else {
      this.style.backgroundColor = this.node.lineDivStemColor;
    }
    UnTip();
  }
  
  this.draw = function(){
    if (this.isVisible) {
      if (!this.isStemInsignificant) {
        var _lineColor = this.tree.treeLineColor;
      }
      else {
        var _lineColor = this.tree.treeLineNotSignificantColor;
      }
      if (!this.lineDivStem) {
        this.lineDivStem = new Object();
      }
      this.drawLineDiv(this.lineDivStem, this.stemX, this.stemY, this.x, this.y, this.tree.treeLineWidth, this.lineDivStemColor, this.tree.canvasDiv);
      this.lineDivStem.lineDiv.onmouseover = this.lineDivStemMouseOver;
      this.lineDivStem.lineDiv.onmouseout = this.lineDivStemMouseOut;
      if (this.descendant && this.descendant != this.lastDescendant) {
        if (!this.lineDivSpan) {
          this.lineDivSpan = new Object();
        }
        this.drawLineDiv(this.lineDivSpan, this.descendant.stemX, this.descendant.stemY, this.lastDescendant.stemX, this.lastDescendant.stemY, this.tree.treeLineWidth, this.lineDivSpanColor);
        this.lineDivSpan.lineDiv.style.zIndex = this.tree.zIndexNormalSpan;
        this.lineDivSpan.lineDiv.onmouseover = this.lineDivSpanMouseOver;
        this.lineDivSpan.lineDiv.onmouseout = this.lineDivSpanMouseOut;
      }
      if (this.lineDivSpanHighlight) {
        this.drawLineDiv(this.lineDivSpanHighlight, this.descendant.stemX, this.descendant.stemY, this.lastDescendant.stemX, this.lastDescendant.stemY, this.tree.treeLineWidth, this.tree.treeLineSelectColor);
      }
      this.positionTextDivs();
      this.drawPointer();
      if (this.labelDiv) {
        this.labelDiv.style.visibility = "visible";
      }
      if (this.buttonDiv) {
        this.buttonDiv.style.visibility = "visible";
        if (this.isCollapsed) {
          this.buttonDiv.style.backgroundColor = this.tree.treeLineColor;
        }
        else {
          this.buttonDiv.style.backgroundColor = "";
        }
      }
      if (this.collapsedDiv) {
        if (this.isCollapsed) {
          this.collapsedDiv.style.visibility = "visible";
        } else {
          this.collapsedDiv.style.visibility = "hidden";
        }
      }
      if (this.lineDivStem) {
        this.lineDivStem.lineDiv.style.visibility = "visible";
        if (this.isCollapsed) {
          this.drawCollapsedLabelDiv(); // collapsedLabeDiv will be created if it does not exist
        }
      }
      if (this.lineDivSpan) {
        if (this.descendant && !this.isCollapsed) {
          this.lineDivSpan.lineDiv.style.visibility = "visible";
        } else {
          this.lineDivSpan.lineDiv.style.visibility = "hidden";
        }
      }
    } else {
      if (this.labelDiv) {
        this.labelDiv.style.visibility = "hidden";
      }
      if (this.buttonDiv) {
        this.buttonDiv.style.visibility = "hidden";
      }
      if (this.collapsedDiv) {
        this.collapsedDiv.style.visibility = "hidden";
      }
      if (this.lineDivStem) {
        this.lineDivStem.lineDiv.style.visibility = "hidden";
      }
      if (this.lineDivSpan) {
        this.lineDivSpan.lineDiv.style.visibility = "hidden";
      }
    }
  }
  
  this.isStemHighlighted = function() {
    return (this.isHighlighted && this.ancestor && this.ancestor.isHighlighted);
  }
  
  this.drawCollapsedLabelDiv = function() {
    if (!this.collapsedDiv) {
      this.collapsedDiv = newickNodeLabelDiv(this, this.tree.canvasDiv);
      this.collapsedDiv.style.zIndex = this.tree.zIndexCollapsedLabel; 
    }
    var _label = "";
    if (this.label != "") {
      _label = "&nbsp;"+this.label+"&nbsp;";    
    }
    if (this.tree.isRootedOnRight) {
      this.collapsedDiv.innerHTML = _label + "&nbsp;[" + this.subtendedTipCount + "]";
    } else {
      this.collapsedDiv.innerHTML = "[" + this.subtendedTipCount + "]&nbsp;" + _label;
    }
    this.collapsedDiv.style.color = this.tree.collapsedLabelColor;
    if (!this.isCollapsed) {
      this.collapsedDiv.style.visibility = "hidden";
    }
    this.positionTextDivs();            
  }
  
  this.pointAt = function() {
    this.isPointedAt = true;
    var _node = this;
    var _nodeToDraw = this;
    while (_node) {
      _node.hasPointedAtDesc = true;
      if (_node.isCollapsed) {
        _nodeToDraw = _node;
      }
      _node = _node.ancestor;
    }
    return (_nodeToDraw);
  }
  
  this.visibleAncestor = function() {
    var _node = this;
    while (_node && !_node.isVisible) {
      _node = _node.ancestor;
    }
    return _node;
  }
  
  this.getPointerX = function() {
    return this.tree.selectX;
  }

  this.getPointerY = function() {
    return this.visibleAncestor().y;
  }
  
  this.getPointerCoords = function(aPtrObj) {
    if (!aPtrObj) {
      aPtrObj = new Object();
    }
    var _visibleNode = this.visibleAncestor();
    var _labelWidth = 0;
    if (_visibleNode.isCollapsed) {
      if (_visibleNode.collapsedDiv) {
        _labelWidth = _visibleNode.collapsedDiv.offsetWidth;
      }
    } else {
      if (_visibleNode.labelDiv) {
        _labelWidth = _visibleNode.labelDiv.offsetWidth;
      }
    }
    if (!this.tree.isRootedOnRight) {
      aPtrObj.xTip = _visibleNode.x + _visibleNode.tree.treeLineWidth + _labelWidth +
        2 * _visibleNode.tree.spacingTipToLabel;
    }
    else {
      aPtrObj.xTip = _visibleNode.x - _labelWidth -
        2 * _visibleNode.tree.spacingTipToLabel - _visibleNode.tree.treeLineWidth;
    }
    aPtrObj.xBase = this.getPointerX();
    aPtrObj.yTip = this.getPointerY();
    aPtrObj.yBase = this.getPointerY();
    return(aPtrObj);
  }
/*                      if (!this.tree.isRootedOnRight) {
        this.drawLineDiv(this.linePointer, this.x + this.tree.treeLineWidth + _labelWidth +
          2 * this.tree.spacingTipToLabel, this.y, this.tree.selectX, this.y, this.tree.treeLineWidth, 
          this.tree.linePointerColor);
      } else {
        this.drawLineDiv(this.linePointer, this.tree.selectX, this.y, this.x - _labelWidth -
          2 * this.tree.spacingTipToLabel - this.tree.treeLineWidth, this.y, this.tree.treeLineWidth, 
          this.tree.linePointerColor);
      } */
  
  this.onMouseOver = function() {
    if (this.isVisible) {
      if (!this.tree.isFrozen) {
        this.tree.highlightToRoot(this);
      }
      if (this.tree.onNodeMouseOver) {
        this.tree.onNodeMouseOver(this);
      }
      else {
        if (this.labelDiv) {
          this.labelDiv.style.color = "red";
        }
        var _tipStr = this.text;
        if (this.nhx_D != "") {
          _tipStr += "<br />D = " + this.nhx_D;
        }
        if (this.nhx_S != "") {
          _tipStr += "<br />S = " + this.nhx_S;
        }
        if (this.nhx_B != "") {
          _tipStr += "<br />B = " + this.nhx_B;
        }
        if (this.tipStr != "") { // FIX this tipStr vs. _tipStr
          phylo_Tip(_tipStr);
        }
      }
    }
  }
  
  this.onMouseOut = function() {
    if (this.isVisible) {
      if (!this.tree.isFrozen) {
        this.tree.clearHighlight();
      }
      if (this.tree.onNodeMouseOut) {
        this.tree.onNodeMouseOut(this);
      }
      else {
        if (this.labelDiv) {
          this.labelDiv.style.color = 'black';
        }
        UnTip();
      }
    }
  }

  this.onClick = function() {
    if (this.descendant) {
      this.tree.collapseNode(this);
    } else { 
//                      if (this.tree.isSelectable) {
//                              this.tree.setNode(this, !this.isInSet);
//                      }
    }
    if (this.tree.onNodeClick) {
      this.tree.onNodeClick(this);
    }
  }
  
  this.onButtonMouseOver = function() {
    if (this.isVisible) {
      this.buttonDiv.style.backgroundColor = this.tree.treeLineColor;
      if (!this.tree.isFrozen) {
        this.tree.highlightToRoot(this);
      }
      if (this.tree.onNodeButtonMouseOver) {
        this.tree.onNodeButtonMouseOver(this);
      }
      else {
        var _tipStr = this.text;
        if (this.nhx_D != "") {
          _tipStr += "<br />D = " + this.nhx_D;
        }
        if (this.nhx_S != "") {
          _tipStr += "<br />S = " + this.nhx_S;
        }
        if (this.nhx_B != "") {
          _tipStr += "<br />B = " + this.nhx_B;
        }
        this._tipStr = "";
        if (this._tipStr != "") {
          phylo_Tip(_tipStr);
        }
      }
    }
  }
  
  this.onButtonMouseOut = function() {
    if (this.isVisible) {
      if (!this.tree.isFrozen) {
        this.tree.clearHighlight();
      }
      if (!this.isCollapsed) {
        this.buttonDiv.style.backgroundColor = "";
      }
      if (this.tree.onNodeButtonMouseOut) {
        this.tree.onNodeButtonMouseOut(this);
      }
      UnTip();
    }
  }
  
  this.onButtonClick = function() {
    if (this.descendant) {
      if (this.tree.isCollapsable) {
        this.tree.collapseNode(this);
      }
    } else { 
      if (this.tree.isSelectable) {
        this.tree.setNode(this, !this.isInSet);
      } 
    }
    if (this.tree.onNodeButtonClick) {
      this.tree.onNodeButtonClick(this);
    }
  }
  
}

function traverseLabelled(aRoot, aCallback) {
  
  function traverse(aNode) {
    if (aNode.label != "") {
      aCallback(aNode);
    }
    if (aNode.descendant) {
      traverse(aNode.descendant);
    }
    if (aNode.sibling) {
      traverse(aNode.sibling);
    }
  }
  
  traverse(aRoot);      
}

function traverseAll(aRoot, aCallback) {
  
  function all(aNode) {
    aCallback(aNode);
    if (aNode.descendant) {
      all(aNode.descendant);
    }
    if (aNode.sibling) {
      all(aNode.sibling);
    }
  }
  
  all(aRoot);   
}

function countTips(aRoot) {
  
  function countNode(aNode) {
    aNode.subtendedTipCount = 0;
    if (aNode.descendant) {
      countNode(aNode.descendant);
      aNode.subtendedTipCount += aNode.descendant.subtendedTipCount;
    } else {
      aNode.subtendedTipCount = 1;
    }
    if (aNode.sibling) {
      countNode(aNode.sibling);
      aNode.sibling.ancestor.subtendedTipCount += aNode.sibling.subtendedTipCount;
    }
  }
  
  countNode(aRoot);
}

/**
 * 
 * newickTreeParse - parses a text string description and returns the newickNode that
 *      is the root of the tree
 * 
 * @param {Object} treeStr - a text string that describes the tree in Newick format
 * 
 */

function newickTreeParse(treeStr) {
  var root = null;
  var ch = " ";
  var chIndex = -1;
  
  function getCh() {
    do {
      chIndex++;        
    } while (chIndex<treeStr.length && treeStr.charAt(chIndex)=="\n" &&
      treeStr.charAt(chIndex)=="\r" && treeStr.charAt(chIndex)=="\t");
    if (chIndex<treeStr.length) {
      ch = treeStr.charAt(chIndex);
    } else {
      ch = ")";  // Close unmatched open parentheses
    }
  }
  
  function processNodeLabel(aNode) {
    var labelStr = "";
    do {
      labelStr += ch;
      getCh();
    } while (!(ch==" " || ch=="(" || ch==")" || ch=="," || ch==":" || ch=="[" || ch==";"));
    labelStr = labelStr.replace(/_/g, " ");
    if (labelStr.indexOf("*LOST") > -1) {
      aNode.label = labelStr;
    }
    else {
      aNode.label = labelStr;
      aNode.text = labelStr;
    }
  }
  
  function processNodeLength(aNode) {
    var lengthStr = "";
    do {
      getCh();
    } while (ch==" ");
    while ((ch>="0" && ch<="9") || ch=="." || ch=="+" || ch=="-" || ch=="E" || ch=="e") {
      lengthStr += ch;
      getCh();
    }
    aNode.length = (+lengthStr);  // Force conversion of string to number               
  }
  
  function processBracketStr(aStr, aNode) {
    var position = aStr.indexOf("&&NHX");
    if (position>-1) {
      nhx_isPresent = true;
      aStr = aStr.substring(position+5);
      if (aStr.charAt(0)==":") {
        aStr = aStr.substring(1);
        var phrase = aStr.split(":");
        for (var i=0; i<phrase.length; i++) {
          var term = phrase[i].split("=");
          switch (term[0]) {
            case "S":
              aNode.nhx_S = term[1];
              break;
            case "D":
              aNode.nhx_D = term[1];
              break;
            case "B":
              aNode.nhx_B = term[1];
              aNode.length = (+aNode.nhx_B);
              break;
            case "Co":
              if (term[1] == "Y") {
                aNode.isCollapsed = true;
              }
          }
        }
      }
    } else {
      aNode.label = aStr;
    }
  }
  
  function processBetweenBrackets(aNode) {
    var bracketStr = "";
    var ofStr = "";
    do {
      getCh();
    } while (ch==" ");
    if (ch >= "0" && ch <= "9") {
      while (ch != "]") {
        bracketStr += ch;
        getCh();
      }
      getCh();
      while (ch != "[") {
        ofStr += ch;
        getCh();
      }
      aNode.label = "["+bracketStr+"]"+ofStr;
      ch = " ";
      chIndex--;  // Back up so that "[" will be read again by ProcessNode()
//alert("Lost: "+bracketStr);                   
    } else {
      while (ch != "]") {
        bracketStr += ch;
        getCh();
      }
      getCh();
      processBracketStr(bracketStr, aNode);
//alert("Bracket: "+bracketStr);                        
    }
  }
  
  function processNode(ancNode) {
    var doingDescendants = false;
    var node = new newickNode();
    node.ancestor = ancNode;
    if (node.ancestor) {
      node.leftId = node.ancestor.rightId;
      node.rightId = node.leftId+1;
      node.ancestor.rightId = node.rightId+1;
    } else {
      node.leftId = 1;
      node.rightId = 2;
    }
    while (ch==" ") {
      getCh();
    }
    while (ch!=";" && ((ch!="," && ch!=")") || doingDescendants)) {
      switch (ch) {
        case "(":
          doingDescendants = true;
          getCh();
          node.descendant = processNode(node);
          node.lastDescendant = node.descendant;
          node.rightId = node.lastDescendant.rightId+1;
          break;
        case ",":
          getCh();
          node.lastDescendant.sibling = processNode(node);
          node.lastDescendant = node.lastDescendant.sibling;
          node.rightId = node.lastDescendant.rightId+1;
          break;
        case ")":
          doingDescendants = false;
          do {
            getCh();
          } while (ch==" ");
          break;
        case ":":
          processNodeLength(node);
          break;
        case "[":
          processBetweenBrackets(node);
          break;
        default:
          if (!(ch==";" || ch=="," || ch=="(" || ch==")")) {
            processNodeLabel(node);
          }
      }
      while (ch==" ") {
        getCh();
      }
      
    }
    return (node);
  }

  function labelNode(aNode) {
    aNode.label = aNode.leftId+"  "+aNode.rightId;
    if (aNode.descendant) {
      labelNode(aNode.descendant);
    }
    if (aNode.sibling) {
      labelNode(aNode.sibling);
    }
  }
  
  root = processNode(null);
  if (root) {
//              labelNode(root);
  }
  return(root);
}

function newickTreeWriteSubtree(aInternalNode) {
  var _treeDescription = "";
  
  function writeNode(aNode) {
    if (aNode.descendant) {
      _treeDescription += "(";
      writeNode(aNode.descendant);
      _treeDescription += ")";
    }
    if (aNode.label) {
      _treeDescription += aNode.label;
    }
    if (aNode.length) {
      _treeDescription += ":"+aNode.length;
    }
    if (aNode.sibling) {
      _treeDescription += ",";
      writeNode(aNode.sibling);
    }
  }
  
  if (aInternalNode) {
    writeNode(aInternalNode);
    return _treeDescription + ";";
  } else {
    return "";
  }
}

function newickTreeLeftIdNode(aRoot, aId) {
  var notFound = true;
  var foundNode = null;
  
  function checkNode(aNode) {
    if (aNode.leftId == aId) {
      foundNode = aNode;
      notFound = false;
    }
    if (notFound && aNode.descendant) {
      checkNode(aNode.descendant);
    }
    if (notFound && aNode.sibling) {
      checkNode(aNode.sibling);
    }           
  }
  
  checkNode(aRoot);
  return(foundNode);
}

function newickTreeAdjustNodeIds(aRoot, aOffset) {
  
  function adjustNodeIds(aNode) {
    aNode.leftId = aNode.leftId + aOffset - 1;
    aNode.rightId = aNode.rightId + aOffset - 1;
    if (aNode.descendant) {
      adjustNodeIds(aNode.descendant);
    }
    if (aNode.sibling) {
      adjustNodeIds(aNode.sibling);
    }
  }
  
  adjustNodeIds(aRoot);
}

/**
 * 
 * newickTreeGraphic - an interactive graphical Newick tree object
 * 
 * @param {Object} aRoot - The root newickNode of the tree
 * @param {Object} aDivId - The ID of the HTML <div> that is the parent of the HTML 
 *   elements drawn to represent the tree
 * 
 */


function newickTreeGraphic(aRoot, aDivId, aOnLoadLabelDiv) {
  this.root = aRoot;
  this.canvasDiv = top.window.document.getElementById(aDivId);
  this.isRootedOnRight = false;
  this.doShowInternalLabels = true;
  this.doExpandZeroLengthBranches = false;
  this.doShowGeneDuplicationEvents = true;
  this.suppressInternalNodeLabels = false;
  this.isFrozen = false;
  this.isUnitLengths = false;
  this.isSelectable = true;
  this.isCollapsable = true;
  this._treeDepth = 0.0;
  this._nodeOrder = 0.0;
  this.fontFamily = "arial";
  this.fontSize = 11;
  this.backgroundColor = "#fbfbfb";
  this.canvasDivWidth = this.canvasDiv.offsetWidth;  
  this.offsetLeftPercent = 0;
  this.treeWidthPercent = 0;
  this.offsetLeftPixels = 0;
  this.treeWidthPixels = 0;
  this.selectX = 0;
  this.marginLeft = 8;
  this.marginRight = 8;
  this.marginTop = 8;
  this.marginBottom = 8;
  this.pointerStripWidth = 20;
  this.spacingTipToLabel = 4;
  this.spacingInterLabel = 3;
  this.treeLineWidth = 3;
  this.treeLineColor = "black";
  this.treeLineSelectColor = "lime";
  this.treeLineHighlightColor = "yellow";
  this.treeLineNotSignificantColor = "darkgray";
  this.labelHeight = this.fontSize+2;
  this.labelButtonSize = Math.round(this.treeLineWidth*3.5);
  this.labelButtonSizeHalf = Math.round(this.labelButtonSize/2)
  this.labelColor = "black";
  this.collapsedLabelColor = "green";
  this.labelHighlightColor = "red";
  this.labelBackgroundColor = "";
  this.labelSelectBackgroundColor = "yellow";
  this.internalLabelColor = "black";
  this.internalLabelHighlightColor = "red";
  this.internalLabelBackgroundColor = "deepskyblue";
  this.internalLabelSelectBackgroundColor = "yellow";
  this.internalLabelNhxDYColor = "lawngreen";
  this.internalLabelNhxDNColor = "hotpink";
  this.selectLineColor = "red";
  this.linePointerColor = "blue";
  var treeLineColor = this.treeLineColor;
//      var treeLineHighlightColor = this.treeLineHighlightColor;
  var treeLineNotSignificantColor = this.treeLineNotSignificantColor;
  var labelColor = this.labelColor;
  var labelHighlightColor = this.labelHighlightColor;
  var labelBackgroundColor = this.labelBackgroundColor;
  var labelSelectBackgroundColor = this.labelSelectBackgroundColor;
  this.selectNodes = new Array();
  this.selectLineDivs = new Array();
  this.leftForTreeBody = this.canvasDivWidth-this.marginLeft-this.pointerStripWidth-
    this.marginRight;
  this.onChange = null;
  this.onNodeClick = null;
  this.onNodeButtonClick = null;
  this.onLoadLabelDiv = null;   
  this.zIndexNormalSpan = "1";
  this.zIndexHighlightSpan = "2";
  this.zIndexLabel = "3";
  this.zIndexCollapsedLabel = "4";
  if (aOnLoadLabelDiv) {
    this.onLoadLabelDiv = aOnLoadLabelDiv;
  }
  
  this.clear = function() {
    while (this.canvasDiv.childNodes.length > 0) {
      this.canvasDiv.removeChild(this.canvasDiv.lastChild);
    }
    this.selectNodes = [];
    this.selectLineDivs = [];
  }
  
  this.setPercentOffsetAndWidth = function(aOffset, aWidth) {
    this.offsetLeftPercent = aOffset;
    this.treeWidthPercent = aWidth;
  }
  
  this.tipAtIndex = function(aIndex) {
    var _count = 0;
    var _found = null;
    
    function traverse(aNode) {
      if (!aNode.descendant) {
        if (_count == aIndex) {
          _found = aNode;
        } else {
          _count++;
        }                       
      }
      if (!_found && aNode.descendant) {
        traverse(aNode.descendant);
      }
      if (!_found && aNode.sibling) {
        traverse(aNode.sibling);
      }
    }
    
    traverse(this.root);
    return(_found);     
  }

  this.loadLabelDivs = function() {
    var _onLoadLabelDiv = this.onLoadLabelDiv;
    
    function loadLabelDiv(aNode) {
      if (aNode.labelDiv) {
        _onLoadLabelDiv(aNode);
      }
      if (aNode.descendant) {
        loadLabelDiv(aNode.descendant);
      }
      if (aNode.sibling) {
        loadLabelDiv(aNode.sibling);
      }
    }
    
    if (this.onLoadLabelDiv) {
      loadLabelDiv(this.root);
    }
  }

  this.findNode = function(aLabelStr) {
    var _foundNode;
    var _isNotFound = true;
    
    function checkNode(aNode) {
      if (aNode.label==aLabelStr) {
        _foundNode = aNode;
        _isNotFound = false;
      }
      if (_isNotFound && aNode.descendant) {
        checkNode(aNode.descendant);
      }
      if (_isNotFound && aNode.sibling) {
        checkNode(aNode.sibling);
      }
    }

    checkNode(this.root);
    return (_foundNode);
  }
  
  this.drawNodeSetPointer = function() {
    var _tree = this;
    var _firstY = Number.MAX_VALUE;
    var _lastY = Number.MIN_VALUE;
    if (!this.isRootedOnRight) {
      var _selectX = this.offsetLeftPixels + this.treeWidthPixels - this.marginRight - this.treeLineWidth;
    } else {
      var _selectX = this.offsetLeftPixels + this.marginRight;
    }
    
    function drawNodePointer(aNode) {
      if (aNode.isInSet) {
        _firstY = Math.min(aNode.y, _firstY);
        _lastY = Math.max(aNode.y, _lastY);
        aNode.drawLabel();
        if (!_tree.isRootedOnRight) {
          _tree.selectLineDivs[_tree.selectLineDivs.length] = 
            new LineDiv(aNode.x + _tree.treeLineWidth + aNode.labelDiv.offsetWidth + 
            2 * _tree.spacingTipToLabel, aNode.y, _selectX, aNode.y, _tree.treeLineWidth, 
            _tree.selectLineColor, _tree.canvasDiv);
        } else {
          _tree.selectLineDivs[_tree.selectLineDivs.length] = 
            new LineDiv(_selectX, aNode.y, aNode.x - aNode.labelDiv.offsetWidth - 
            2 * _tree.spacingTipToLabel -_tree.treeLineWidth, aNode.y, _tree.treeLineWidth, 
            _tree.selectLineColor, _tree.canvasDiv);
        }
      }
      if (aNode.descendant) {
        drawNodePointer(aNode.descendant);
      }
      if (aNode.sibling) {
        drawNodePointer(aNode.sibling);
      }
    }
    
    drawNodePointer(this.root);         
    if (_firstY!=Number.MAX_VALUE && _firstY!=_lastY) {
      this.selectLineDivs[this.selectLineDivs.length] = new
        LineDiv(_selectX, _firstY, _selectX, _lastY, this.treeLineWidth,
        this.selectLineColor, this.canvasDiv);
    }
  }
  
  this.clearNodeSetPointer = function() {
    for (var i=0; i<this.selectLineDivs.length; i++) {
      this.selectLineDivs[i].lineDiv.style.visibility = "hidden";
    }
  }
  
  this.drawHighlightNodeSet = function() {
    this.highlightNodeSet();
    this.drawNodeSetPointer();
    this.isFrozen = true;
  }
  
  this.selectLabelSet = function(aSet) {
    var _firstY = Number.MAX_VALUE;
    var _lastY = Number.MIN_VALUE;
    for (var i = 0; i < aSet.length; i++) {
      var _node = this.findNode(aSet[i]);
      if (_node) {
        _node.isInSet = true;
      }
    }

    this.highlightNodeSet();
    this.drawNodeSetPointer();
    this.isFrozen = true;
  }
  
  this.setNode = function(aNode, aIsToBeAdded) {
    this.clearHighlight();
    this.clearNodeSetPointer();
    aNode.isInSet = aIsToBeAdded;
    aNode.drawLabel();
    this.highlightNodeSet();
    this.drawNodeSetPointer();
    if (aIsToBeAdded) {
      this.isFrozen = true;
    }
  }
  
  this.clearSelect = function() {
    var _labelBackgroundColor = this.labelBackgroundColor;
    
    function clearNodeSet(aNode) {
      aNode.isInSet = false;
      if (aNode.labelDiv) {
        aNode.labelDiv.style.backgroundColor = _labelBackgroundColor;
      }
      if (aNode.descendant) {
        clearNodeSet(aNode.descendant); 
      }
      if (aNode.sibling) {
        clearNodeSet(aNode.sibling);    
      }
    }
    
    this.selectNodes = [];
    for (var i=0; i<this.selectLineDivs.length; i++) {
      this.canvasDiv.removeChild(this.selectLineDivs[i].lineDiv);
    }
    this.selectLineDivs = [];
    clearNodeSet(this.root);
    this.clearHighlight();
    this.isFrozen = false;      
  }
  
  this.colorSubtree = function(aNode, aColor) {
    
    function colorNode(_node) {
      if (_node != aNode) {
        _node.lineDivStemColor = aColor;
        if (_node.lineDivStem && !_node.isStemHighlighted()) {
          _node.lineDivStem.lineDiv.style.backgroundColor = aColor;
        }
      }
      _node.lineDivSpanColor = aColor;
      if (_node.lineDivSpan) {
        _node.lineDivSpan.lineDiv.style.backgroundColor = aColor;
      }
      if (_node.descendant) {
        colorNode(_node.descendant);
      }
      if (_node != aNode && _node.sibling) {
        colorNode(_node.sibling);
      }
    }
    
    colorNode(aNode);
  }

  this.clearColorSubtrees = function() {
    var _treeLineColor = this.treeLineColor;
    
    function clearColor(aNode) {
      aNode.lineDivStemColor = _treeLineColor;
      if (aNode.lineDivStem && !aNode.isStemHighlighted()) {
        aNode.lineDivStem.lineDiv.style.backgroundColor = _treeLineColor;
      }
      aNode.lineDivSpanColor = _treeLineColor;
      if (aNode.lineDivSpan) {
        aNode.lineDivSpan.lineDiv.style.backgroundColor = _treeLineColor;
      }
      if (aNode.descendant) {
        clearColor(aNode.descendant);
      }
      if (aNode.sibling) {
        clearColor(aNode.sibling);
      }                 
    }
    
  clearColor(this.root);        
  }
  
  this.drawHighlight = function(aIsBranch) {
    var _tree = this;
    var _selectColor = this.treeLineSelectColor;
    var _highlightColor = this.treeLineHighlightColor;
    
    function drawNodeHighlight(aNode) {
      if (aNode.isHighlighted && aNode.isVisible) {
        if (aNode.lineDivStem && (aIsBranch || (aNode.ancestor && 
            aNode.ancestor.isHighlighted))) {
          aNode.lineDivStem.lineDiv.style.backgroundColor = _selectColor;
          aNode.lineDivStem.lineDiv.normalColor = _selectColor;
        }
        if (aNode.descendant) {
          var _minY = aNode.y;
          var _maxY = _minY;
          var _n = aNode.descendant;
          while (_n) {
            if (_n.isHighlighted) {
              _minY = Math.min(_n.y, _minY);
              _maxY = Math.max(_n.y, _maxY);
            }
            _n = _n.sibling;
          }
          if (aNode.lineDivSpanHighlight) {
            aNode.drawLineDiv(aNode.lineDivSpanHighlight, aNode.x, _minY, 
              aNode.x, _maxY, _tree.treeLineWidth, _selectColor);       
            aNode.lineDivSpanHighlight.lineDiv.style.visibility = "visible";
          }
          else if (!aNode.isCollapsed) {
            aNode.lineDivSpanHighlight = new Object();
            aNode.drawLineDiv(aNode.lineDivSpanHighlight, aNode.x, _minY, 
              aNode.x, _maxY, _tree.treeLineWidth, _selectColor);       
            aNode.lineDivSpanHighlight.lineDiv.node = aNode;  
            aNode.lineDivSpanHighlight.lineDiv.normalColor = _selectColor;  
            aNode.lineDivSpanHighlight.lineDiv.highlightColor = _highlightColor;  
            aNode.lineDivSpanHighlight.lineDiv.style.zIndex = _tree.zIndexHighlightSpan;
            aNode.lineDivSpanHighlight.lineDiv.onmouseover = function() {
              this.style.backgroundColor = this.highlightColor;
              phylo_Tip("Level:&nbsp;"+this.node.lengthFromRoot);
            }
            aNode.lineDivSpanHighlight.lineDiv.onmouseout = function() {
              this.style.backgroundColor = this.normalColor;
              UnTip();
            }
          }
        }
      }
      if (aNode.descendant) {
        drawNodeHighlight(aNode.descendant);
      }
      if (aNode.sibling) {
        drawNodeHighlight(aNode.sibling);
      }
    }
    
    drawNodeHighlight(this.root);
  }
  
  this.clearHighlight = function() {
    var _treeLineColor = this.treeLineColor;
    var _treeLineNotSignificantColor = this.treeLineNotSignificantColor;
    
    function clearNodeHighlight(aNode) {
      if (aNode.isHighlighted) {
        if (aNode.lineDivStem) {
          if (!aNode.isStemInsignificant) {
            var _color = _treeLineColor;
          } else {
            var _color = _treeLineNotSignificantColor;
          }
          aNode.lineDivStem.lineDiv.style.backgroundColor = aNode.lineDivStemColor;
          aNode.lineDivStem.lineDiv.normalColor = aNode.lineStemDivColor;
          if (aNode.lineDivSpanHighlight) {
            aNode.lineDivSpanHighlight.lineDiv.style.visibility = "hidden";
          }
          aNode.isHighlighted = false;
          aNode.drawLabel();
        }
      }
      if (aNode.descendant) {
        clearNodeHighlight(aNode.descendant);
      }
      if (aNode.sibling) {
        clearNodeHighlight(aNode.sibling);
      }
    }
    
    clearNodeHighlight(this.root);
    this.drawHighlight(true);
  }
  
  this.highlightToRoot = function(aNode) {
    n = aNode;
    while (n) {
      n.isHighlighted = true;
      n = n.ancestor;
    }
    this.drawHighlight(true);   
  }
  
  this.highlightNodeSet = function() {
    
    function highlightNode(aNode) {
      if (aNode.isInSet) {
        aNode.isHighlighted = true;
      } 
      if (aNode.descendant) {
        highlightNode(aNode.descendant);
        var _n = aNode.descendant;
        while (_n) {
          if (_n.isHighlighted) {
            aNode.isHighlighted = true;
            break;
          }
          _n = _n.sibling;
        }
      }
      if (aNode.sibling) {
        highlightNode(aNode.sibling);
      }
    }
    
    function removeSingleStem(aNode) {
      if (aNode.isHighlighted) {
        if (aNode.descendant) {
          var _n = aNode.descendant;
          var _count = 0;
          while (_n) {
            if (_n.isHighlighted) {
              _count++;
            }
            _n = _n.sibling;
          }
          if (_count<2) {
            aNode.isHighlighted = false;
            removeSingleStem(aNode.descendant);
          }
        }
      }
      if (aNode.sibling) {
        removeSingleStem(aNode.sibling);
      }
    }
    
    highlightNode(this.root);
    removeSingleStem(this.root);
    this.drawHighlight(false);
  }
  
  this.findLabels = function(aStr) {
    var _tree = this;
    var _count = 0;
    
    function findLabelNode(aNode) {
      if (aNode.label.indexOf(aStr)>=0) {
        aNode.isInSet = true;
        aNode.drawLabel();
        _count++;
      }
      if (aNode.descendant) {
        findLabelNode(aNode.descendant);
      }
      if (aNode.sibling) {
        findLabelNode(aNode.sibling);
      }
    }
    
    if (aStr && aStr!="") {
      findLabelNode(this.root);
      if (_count>0) {
        this.highlightNodeSet();
        this.isFrozen = true;
        this.drawNodeSetPointer();
      }
    }
    return (_count);
  }
  
  this.scrollWindowToLabel = function(aLabelStr) {
    var _theFoundNode;
    
    function getWindowHeight() {
      return  top.window.innerHeight != null ? 
        window.innerHeight : document.documentElement && 
        document.documentElement.clientHeight ? 
        document.documentElement.clientHeight : 
        document.body != null ? document.body.clientHeight : null;
    }           
    
    function getElementPosition(aElement) {
      var offsetTrail = aElement;
      var offsetLeft = 0;
      var offsetTop = 0;
      while (offsetTrail) {
        offsetLeft += offsetTrail.offsetLeft;
        offsetTop += offsetTrail.offsetTop;
        offsetTrail = offsetTrail.offsetParent;
      }
      return {left:offsetLeft, top:offsetTop};
    }
    
    _theFoundNode = this.findNode(aLabelStr);
    if (_theFoundNode) {
      top.window.scrollTo(0, Math.max(0, getElementPosition(_theFoundNode.labelDiv).top)-
        Math.round(getWindowHeight()/2));               
      _theFoundNode.labelDiv.style.backgroundColor = "yellow";
      return (true);
    } else {
      return (false);
    }
  }
  
  this.setUnitLengths = function(aBooleanValue) {
    if (aBooleanValue!=this.isUnitLengths) {
      this.isUnitLengths = aBooleanValue;
      this.drawTree();
    }
  }

  this.collapseNode = function(aNode){
    if (aNode.descendant) {
      aNode.isCollapsed = !aNode.isCollapsed;
      if (!aNode.isCollapsed && aNode.collapsedDiv) {
        aNode.collapsedDiv.style.visibility = "hidden";
      }
      if (!aNode.isCollapsed && aNode.buttonDiv) {
        aNode.buttonDiv.style.visibility = "hidden";
      }
      this.draw();
      if (this.onChange) {
        this.onChange(this);
      }
    }
  }
  
  this.expandAll = function() {
    
    function expand(aNode) {
      aNode.isCollapsed = false;
      if (aNode.descendant) {
        expand(aNode.descendant);
      }
      if (aNode.sibling) {
        expand(aNode.sibling);
      }                 
    }
    
  expand(this.root);
  }

// Basic drawing functions
  
  this.setTreePropertyOfNodes = function() {
    var _tree = this;
    
    function setTreeProperty(aNode) {
      aNode.tree = _tree;
      aNode.lineDivStemColor = _tree.treeLineColor;
      aNode.lineDivSpanColor = _tree.treeLineColor;
      if (aNode.descendant) {
        setTreeProperty(aNode.descendant);
      }
      if (aNode.sibling) {
        setTreeProperty(aNode.sibling);
      }                 
    }
    
  setTreeProperty(this.root);   
  }
  
  this.createNodeLabelDivs = function(aNode) {
    aNode.createLabel();
    if (aNode.descendant) {
      this.createNodeLabelDivs(aNode.descendant);
    }
    if (aNode.sibling) {
      this.createNodeLabelDivs(aNode.sibling);
    } 
  }

  this.layoutTree = function(aNode) {
    aNode.isVisible = (!aNode.ancestor || (aNode.ancestor.isVisible && !aNode.ancestor.isCollapsed));
    if (!this.isUnitLengths && aNode.length==0 && aNode.ancestor && this.doExpandZeroLengthBranches) {
      aNode.length = 0.05;
      aNode.isStemInsignificant = true;
    }
    var _length = aNode.length;
    if (this.isUnitLengths) {
      _length = 1;
    }
    if (aNode.ancestor) {
      aNode.lengthFromRoot = aNode.ancestor.lengthFromRoot+_length;
    } else {
      aNode.lengthFromRoot = _length;
    }
    if (this._treeDepth<aNode.lengthFromRoot) {
      this._treeDepth = aNode.lengthFromRoot;
    }
    if (aNode.descendant) {
      this.layoutTree(aNode.descendant);
      aNode.subtendedTipCount = 0;
      var n = aNode.descendant;
      while (n) {
        aNode.subtendedTipCount += n.subtendedTipCount;
        n = n.sibling;
      }
      if (aNode.isVisible) {
        if (aNode.isCollapsed) {
          aNode.order = this._nodeOrder;
          this._nodeOrder++;
        } else {
          aNode.order = (aNode.descendant.order + aNode.lastDescendant.order) / 2;
        }
      }
    } else {
      aNode.subtendedTipCount = 1;
      if (aNode.isVisible) {
        aNode.order = this._nodeOrder;
        this._nodeOrder++;
      }
    }
    if (aNode.sibling) {
      this.layoutTree(aNode.sibling);
    }
  }
  
//  Scale Node.level so that tree root level is 0.0 and farthest tip level is 1.0
  this.scaleNodeLevel = function(aNode) {
    aNode.level = aNode.lengthFromRoot/this._treeDepth;
    if (!this.isUnitLengths) {
      aNode.levelLength = aNode.length / this._treeDepth;
    } else {
      aNode.levelLength = 1 / this._treeDepth;
    }
    if (aNode.descendant) {
      this.scaleNodeLevel(aNode.descendant);
    }
    if (aNode.sibling) {
      this.scaleNodeLevel(aNode.sibling);
    }
  }
  
  this.setCanvasDivStyle = function(aRoot) {
    this.canvasDiv.style.fontFamily = this.fontFamily;
    this.canvasDiv.style.fontSize = this.fontSize+"px";
    this.canvasDiv.style.backgroundColor = this.backgroundColor; 
    var _thisHeight = aRoot.subtendedTipCount*(this.labelHeight+this.spacingInterLabel)-
      this.spacingInterLabel+this.treeLineWidth+this.marginTop+this.marginBottom;
    if (_thisHeight > parseInt(this.canvasDiv.offsetHeight)) {
      this.canvasDiv.style.height = _thisHeight + "px";
    }
  }
    
  this.placeAllTreeElements = function(aRoot) {
    var _minTreeDepthScaling = Number.MAX_VALUE;
    
    this.calcPixelOffsetsFromPercents = function() {
      if (this.offsetLeftPercent != 0 || this.treeWidthPercent != 0) {
        this.offsetLeftPixels = this.canvasDivWidth * this.offsetLeftPercent / 100.0;   
        this.treeWidthPixels = this.canvasDivWidth * this.treeWidthPercent / 100.0;     
        this.leftForTreeBody = this.treeWidthPixels - 
          this.marginLeft - this.pointerStripWidth - this.marginRight;
      }
      if (!this.isRootedOnRight) {
        this.selectX = this.offsetLeftPixels + this.treeWidthPixels - this.marginRight - this.treeLineWidth;
      } else {
        this.selectX = this.offsetLeftPixels + this.marginRight;
      }
    }

    this.calcTreeDepthScaling = function(aNode) {
      var _leftForTreeBody = this.leftForTreeBody;
      if (aNode.labelDiv) {
        _leftForTreeBody -= this.spacingTipToLabel+aNode.labelDiv.offsetWidth;
      }
      if (_minTreeDepthScaling>_leftForTreeBody/aNode.level) {
        _minTreeDepthScaling = _leftForTreeBody/aNode.level;    
      }
      if (aNode.descendant) {
        this.calcTreeDepthScaling(aNode.descendant);
      }
      if (aNode.sibling) {
        this.calcTreeDepthScaling(aNode.sibling);
      }
    }
    
    this.calcTreeElementCoordinates = function(aNode) {
      if (!this.isRootedOnRight) {
        aNode.x = this.offsetLeftPixels + this.marginLeft + Math.round(aNode.level * _minTreeDepthScaling);
        aNode.stemX = this.offsetLeftPixels + this.marginLeft + Math.round((aNode.level - aNode.levelLength) * 
          _minTreeDepthScaling);
      } else {
        aNode.x = this.offsetLeftPixels + this.treeWidthPixels - this.marginRight - Math.round(aNode.level * _minTreeDepthScaling);
        aNode.stemX = this.offsetLeftPixels + this.treeWidthPixels - this.marginRight - Math.round((aNode.level - aNode.levelLength) * 
          _minTreeDepthScaling);
      }
      aNode.y = Math.round((aNode.order * (this.labelHeight + this.spacingInterLabel)) +
        this.marginTop + (this.labelHeight / 2));
      aNode.stemY = aNode.y;
      aNode.positionTextDivs();
      if (aNode.descendant) {
        this.calcTreeElementCoordinates(aNode.descendant);
      }
      if (aNode.sibling) {
        this.calcTreeElementCoordinates(aNode.sibling);
      }
    }

    this.calcPixelOffsetsFromPercents();
    this.calcTreeDepthScaling(aRoot);
    this.calcTreeElementCoordinates(aRoot);
  }
  
  this.drawNodes = function(aNode) {
    aNode.draw();
    if (aNode.descendant) {
      this.drawNodes(aNode.descendant);
    }
    if (aNode.sibling) {
      this.drawNodes(aNode.sibling);
    }
  }     

  this.draw = function() {
    this.clearHighlight();
    this.clearSelect();
    this.createNodeLabelDivs(this.root);
    this._nodeOrder = 0.0;
    this.layoutTree(this.root);
    this.scaleNodeLevel(this.root);
    this.setCanvasDivStyle(this.root);
    this.placeAllTreeElements(this.root);
    this.drawNodes(this.root);
  }     
  
  this.setTreePropertyOfNodes();
}
