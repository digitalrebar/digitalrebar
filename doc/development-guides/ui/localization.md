### Adding Localizations (i18n)

OpenCrowbar uses the Rails I18N library.  Please refer to the documentation [http://guides.rubyonrails.org/i18n.html](http://guides.rubyonrails.org/i18n.html) for usage hints that can help you reduce coding, and to add features such as Interpolation.

Each barclamp is expected to add its own localization (i18n) file.
>**Note**: Please do _not_ add your localizations into another barclamp's i18n file.
You must also be careful not to create duplicate entries; doing so can confuse OpenCrowbar.

1. Add your localization file (`en.yml` is the default) into the `crowbar_framework/config/locales/[barclamp]` directory.
>**Note**: You must replace [barclamp] with the name of your barclamp.

1. If you are supporting multiple languages, replace `en` with the target language code.  For example, use `fr.yml` if you want to provide French translations.

1. Inside the i18n file, provide a simple YML hash for translations. For example:

    <pre>en:
      # Layout
      nav:
        nodes: Nodes
        nodes_description: Infrastructure Components</pre>

>**Reminder**: Encode your translations in quotes if you need to use comma ( : ) or tick ( ` ) characters!
