import type { PickerOperatorOption } from "../types";

type CoordinatePickerOperator =
  | "="
  | "!="
  | ">"
  | "<"
  | "between"
  | "inside"
  | ">="
  | "<=";

export interface OperatorOption
  extends PickerOperatorOption<CoordinatePickerOperator> {
  valueCount: number;
}