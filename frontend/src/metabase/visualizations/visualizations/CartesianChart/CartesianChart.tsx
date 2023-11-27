import { useMemo } from "react";
import { color } from "metabase/lib/colors";
import { formatValue } from "metabase/lib/formatting/value";
import { measureTextWidth } from "metabase/lib/measure-text";
import { getCartesianChartModel } from "metabase/visualizations/echarts/cartesian/model";
import type {
  RenderingContext,
  VisualizationProps,
} from "metabase/visualizations/types";
import { EChartsRenderer } from "metabase/visualizations/components/EChartsRenderer";
import { getCartesianChartOption } from "metabase/visualizations/echarts/cartesian/option";
import { computeStaticComboChartSettings } from "metabase/static-viz/components/ComboChart/settings";

export function CartesianChart({
  width,
  height,
  settings,
  rawSeries,
  fontFamily,
}: VisualizationProps) {
  const renderingContext: RenderingContext = useMemo(
    () => ({
      getColor: color,
      formatValue: (value, options) => String(formatValue(value, options)),
      measureText: measureTextWidth,
      fontFamily: fontFamily,
    }),
    [fontFamily],
  );

  // TODO: remove once viz settings definitions are updated
  settings = computeStaticComboChartSettings(
    rawSeries,
    settings,
    renderingContext,
  );

  const chartModel = useMemo(
    () => getCartesianChartModel(rawSeries, settings, renderingContext),
    [rawSeries, renderingContext, settings],
  );
  const option = useMemo(
    () => getCartesianChartOption(chartModel, settings, renderingContext),
    [chartModel, renderingContext, settings],
  );

  return <EChartsRenderer width={width} height={height} option={option} />;
}
