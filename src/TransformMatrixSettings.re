open Canvas;
open Layer;

let numericTextFieldStyle =
  ReactDOMRe.Style.make(~width="45%", ~marginRight="5%", ());

let component = ReasonReact.statelessComponent(__MODULE__);

let make = (~layer, ~changeLayer, _children) => {
  ...component,
  render: self =>
    MaterialUi.(
      <div>
        <FormGroup row=true>
          <NumericTextField
            style=numericTextFieldStyle
            margin=`Dense
            label=(ReasonReact.string("x"))
            value=(`Float(layer.transformMatrix.horizontalMoving))
            onChange=(
              newX =>
                changeLayer(
                  layer,
                  Some({
                    ...layer,
                    transformMatrix: {
                      ...layer.transformMatrix,
                      horizontalMoving: newX,
                    },
                  }),
                )
            )
          />
          <NumericTextField
            style=numericTextFieldStyle
            margin=`Dense
            label=(ReasonReact.string("y"))
            value=(`Float(layer.transformMatrix.verticalMoving))
            onChange=(
              newY =>
                changeLayer(
                  layer,
                  Some({
                    ...layer,
                    transformMatrix: {
                      ...layer.transformMatrix,
                      verticalMoving: newY,
                    },
                  }),
                )
            )
          />
        </FormGroup>
        <FormGroup row=true>
          <NumericTextField
            style=numericTextFieldStyle
            margin=`Dense
            label=(ReasonReact.string("scaleX"))
            value=(`Float(layer.transformMatrix.horizontalScaling))
            onChange=(
              newScaleX =>
                changeLayer(
                  layer,
                  Some({
                    ...layer,
                    transformMatrix: {
                      ...layer.transformMatrix,
                      horizontalScaling: newScaleX,
                    },
                  }),
                )
            )
          />
          <NumericTextField
            style=numericTextFieldStyle
            margin=`Dense
            label=(ReasonReact.string("scaleY"))
            value=(`Float(layer.transformMatrix.verticalScaling))
            onChange=(
              newScaleY =>
                changeLayer(
                  layer,
                  Some({
                    ...layer,
                    transformMatrix: {
                      ...layer.transformMatrix,
                      verticalScaling: newScaleY,
                    },
                  }),
                )
            )
          />
        </FormGroup>
        <FormGroup row=true>
          <NumericTextField
            style=numericTextFieldStyle
            margin=`Dense
            label=(ReasonReact.string("skewX"))
            value=(`Float(layer.transformMatrix.horizontalSkewing))
            onChange=(
              newSkewX =>
                changeLayer(
                  layer,
                  Some({
                    ...layer,
                    transformMatrix: {
                      ...layer.transformMatrix,
                      horizontalSkewing: newSkewX,
                    },
                  }),
                )
            )
          />
          <NumericTextField
            style=numericTextFieldStyle
            margin=`Dense
            label=(ReasonReact.string("skewY"))
            value=(`Float(layer.transformMatrix.verticalSkewing))
            onChange=(
              newSkewY =>
                changeLayer(
                  layer,
                  Some({
                    ...layer,
                    transformMatrix: {
                      ...layer.transformMatrix,
                      verticalSkewing: newSkewY,
                    },
                  }),
                )
            )
          />
        </FormGroup>
      </div>
    ),
};
