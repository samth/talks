#!/usr/bin/env python

import poppler
import gtk
import urllib
import sys, os

if len(sys.argv) != 2:
    print("Usage: %s <filename>")
    sys.exit()

input_filename = os.path.abspath(sys.argv[1])
output_filename = os.path.splitext(os.path.basename(sys.argv[1]))[0] + '-%.2d.png'
width = 768
height = 576

doc = poppler.document_new_from_file('file://%s' % \
            urllib.pathname2url(input_filename), password=None)

for i in xrange(doc.get_n_pages()):
        page = doc.get_page(i)
        pixbuf = gtk.gdk.Pixbuf(gtk.gdk.COLORSPACE_RGB, True, 8, width, height)
        page.render_to_pixbuf(src_x=0, src_y=0, src_width=width, src_height=height,
                    scale=width/page.get_size()[0], rotation=0, pixbuf=pixbuf)
        pixbuf.save(output_filename % i, 'png')

