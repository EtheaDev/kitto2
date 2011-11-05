To use the HelloKitto database you need to:

1) Setup Firebird 2.5 with a database alias "HelloKitto" defined in aliases.conf.
2) Restore DB\HelloKitto.fbk on a folder of your choosing and have the alias point to the file.
3) Optionally edit database connection parameters in Home\Metadata\Config.yaml and Home\Metadata\ConfigViewport.yaml.

To see the most minimal Kitto application, the classic "Hello World" example, use ConfigHelloWorld.yaml.

To see the application in stand-alone mode, use Config.yaml.

To see the application embedded in a web page, use ConfigViewport.yaml and access through an URL like:

http://localhost/HelloKitto/EmbeddedTest.html
