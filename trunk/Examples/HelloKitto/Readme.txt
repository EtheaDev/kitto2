Database setup
--------------
To use the HelloKitto database you need to:

1) Setup Firebird 2.5 with a database alias "HelloKitto" defined in aliases.conf.
2) Restore DB\HelloKitto_Firebird.fbk on a folder of your choosing and have the alias point to the file.
3) Optionally edit database connection parameters in Home\Metadata\Config.yaml.

Standard mode
-------------
To see the application in stand-alone mode, use the standard Config.yaml and the standard URL such as:

http://localhost/kitto/hellokitto

Embedded mode
-------------
Embedded mode allows to embed one or more portions of a Kitto application in your own web page. Please have
a look at the file Home\Resources\EmbeddedTest.html for an example that embeds two views from the application
in a very simple web page.

For embedded mode, use an URL like:
http://localhost/HelloKitto/EmbeddedTest.html

Hello World
-----------
To see the most minimal Kitto application, the classic "Hello World" example, use a custom URL such as:

http://localhost/kitto/hellokitto?home=HelloWorld
