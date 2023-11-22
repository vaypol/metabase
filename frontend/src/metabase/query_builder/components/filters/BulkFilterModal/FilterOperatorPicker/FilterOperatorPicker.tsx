import { useMemo } from "react";
import { t } from "ttag";
import { Button, Menu } from "metabase/ui";
import {
  DropdownIcon,
  SelectableMenuItem,
} from "./FilterOperatorPicker.styled";

type Option<T> = {
  name: string;
  operator: T;
};

interface FilterOperatorPickerProps<T> {
  value: T;
  options: Option<T>[];
  disabled?: boolean;
  onChange: (operator: T) => void;
}

export function FilterOperatorPicker<T extends string>({
  value,
  options,
  disabled,
  onChange,
}: FilterOperatorPickerProps<T>) {
  const label = useMemo(() => {
    const option = options.find(option => option.operator === value);
    return option ? option.name.toLowerCase() : t`operator`;
  }, [value, options]);

  return (
    <Menu position="bottom-start">
      <Menu.Target>
        <Button
          variant="subtle"
          disabled={disabled}
          color="brand.1"
          td={disabled ? "none" : "underline"}
          p="xs"
          aria-label={t`Filter operator`}
        >
          {label}
          {!disabled && <DropdownIcon name="chevrondown" size={8} />}
        </Button>
      </Menu.Target>
      <Menu.Dropdown>
        {options.map(option => (
          <SelectableMenuItem
            key={option.operator}
            aria-selected={option.operator === value}
            onClick={() => onChange(option.operator)}
          >
            {option.name}
          </SelectableMenuItem>
        ))}
      </Menu.Dropdown>
    </Menu>
  );
}
