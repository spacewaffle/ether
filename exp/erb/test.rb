require 'erb'
require 'sequel'

user_id = ARGV[0]

DB = Sequel.connect("mysql2://root@localhost/redesign")

user = DB[:users][user_id]
puts user.inspect

puts ERB.new(File.read('login.erb')).result(binding)
