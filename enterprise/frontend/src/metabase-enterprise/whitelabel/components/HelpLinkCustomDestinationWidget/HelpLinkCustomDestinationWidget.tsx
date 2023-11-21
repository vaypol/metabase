import InputWithSelectPrefix from "metabase/components/InputWithSelectPrefix";

export const HelpLinkCustomDestinationWidget = ({
  setting,
  onChange,
  ...props
}: {
  setting: { value: string; placeholder: string };
  onChange: (value: string) => void;
}) => {
  return (
    <InputWithSelectPrefix
      value={setting.value}
      onChange={(e: { target: { value: string } }) => onChange(e.target.value)}
      prefixes={["https://", "http://", "mailto:"]}
      defaultPrefix="https://"
      caseInsensitivePrefix={true}
      placeholder={setting.placeholder}
    />
  );
};
