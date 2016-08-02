#!/usr/bin/env ruby

require 'net/http'
require 'rexml/document'
class ParseRss
	def initialize(url)
		@url = url
	end
	
	def parse
		@content = Net::HTTP.get(URI.parse(@url))
		xml = REXML::Document.new(@content)
		data = {}
		data['title'] = xml.root.elements['channel/title'].text
		data['home_url'] = xml.root.elements['channel/link'].text
		data['rss_url'] = @url
		data['items'] = []
		xml.elements.each('//item') do |item|
			it = {}
			it['title'] = item.elements['title'].text
			it['link'] = item.elements['link'].text
			it['description'] = item.elements['description'].text
			if item.elements['dc:creator']
				it['author'] = item.elements['dc:creator'].text
			end
			if item.elements['dc:date']
				it['publication_date'] = item.elements['dc:date'].text
			elsif item.elements['pubDate']
				it['publication_date'] = item.elements['pubDate'].text
			end
			data['items'] << it
		end
		data
	end
end

data = ParseRss.new('http://rss.wunderground.com/auto/rss_full/DC/Washington.xml?units=both').parse

data['items'][0]['title'] =~ /(\d+F)/
print $1
