__author__ = 'n'
import numpy
import matplotlib
import matplotlib.pyplot
import numpy.lib.recfunctions as nr
import bisect
import time

# rename the files below to correct file names
student_record_file = 'd:/umd lara data/md_sr_nb_2.csv'
student_courses_file = 'd:/umd lara data/md_sc_short_nb_2.csv'

def load_student_record():
	sr = numpy.loadtxt(student_record_file,delimiter=',',skiprows=1, \
		dtype=[('id','i6'), ('gender', 'a1'), ('ethnicity', 'i1'), \
		('first_term', 'a6'), ('degree_term', 'a6'), ('transfer', 'a1'), \
		('major1', 'a10'), ('major2', 'a10'), ('major1_long', 'a30'),
		('major2_long', 'a30'), ('first_declare', 'a10'), ('first_declare_long', 'a30'), \
		('first_declare_term', 'a6'), ('pell_status', 'a1')])

	nstudents = len(sr['id'])
	ct = numpy.recarray((nstudents,),dtype=[('id','i6'), ('gender', 'a1'), ('ethnicity', 'i1'), \
		('first_term', 'i6'), ('degree_term', 'i6'), ('transfer', 'a1'), \
		('major1', 'a10'), ('major2', 'a10'), ('major1_long', 'a30'),
		('major2_long', 'a30'), ('first_declare', 'a10'), ('first_declare_long', 'a30'), \
		('first_declare_term', 'i6'), ('pell_status', 'a1')])

	ct.id = sr['id']
	ct.gender = sr['gender']
	ct.ethnicity = sr['ethnicity']
	ct.first_term = sr['first_term']
	ct.degree_term = sr['degree_term']
	ct.transfer = sr['transfer']
	ct.major1 = sr['major1']
	ct.major2 = sr['major2']
	ct.major1_long = sr['major1_long']
	ct.major2_long = sr['major2_long']
	ct.first_declare = sr['first_declare']
	ct.first_declare_long = sr['first_declare_long']
	good = numpy.where(sr['first_declare_term'] != 'NA')
	bad = numpy.where(sr['first_declare_term'] == 'NA')
	ct.first_declare_term[good] = sr['first_declare_term'][good]
	ct.first_declare_term[bad] = 0
	ct.pell_status = sr['pell_status']

	return ct
	end

def load_student_course():
	sr = numpy.loadtxt(student_courses_file,delimiter=',',skiprows=1, \
		dtype=[('id','i6'), ('subject', 'a8'), ('catalognbr', 'i3'), \
		('course_code', 'a12'), ('grade', 'f4'), ('gpao', 'f4'), \
		('cum_gpa', 'a10'), ('totalcredits', 'f4'), ('totalgradepoints', 'f4'),
		('coursecredit', 'i6'), ('term', 'i6')])

	nstudents = len(sr['id'])
	ct = numpy.recarray((nstudents,),dtype=[('id','i6'), ('subject', 'a8'), ('catalognbr', 'i3'), \
		('course_code', 'a12'), ('grade', 'f4'), ('gpao', 'f4'), \
		('cum_gpa', 'f4'), ('totalcredits', 'f4'), ('totalgradepoints', 'f4'),
		('coursecredit', 'i6'), ('term', 'i6')])

	ct.id = sr['id']
	ct.subject = sr['subject']
	ct.catalognbr = sr['catalognbr']
	ct.course_code = sr['course_code']
	ct.grade = sr['grade']
	ct.gpao = sr['gpao']
	good = numpy.where(sr['cum_gpa'] != 'NA')
	bad = numpy.where(sr['cum_gpa'] == 'NA')
	ct.cum_gpa[good] = sr['cum_gpa'][good]
	ct.cum_gpa[bad] = 0
	ct.totalcredits = sr['totalcredits']
	ct.totalgradepoints = sr['totalgradepoints']
	ct.coursecredit = sr['coursecredit']
	ct.term = sr['term']

	return ct
	end

def combine_sr_sct_information(sr,sct):

	nd=nr.append_fields(sct,'gender',numpy.zeros(sct.shape[0]),dtypes='a1').view(numpy.recarray)
	nd=nr.append_fields(nd,'ethnicity',numpy.zeros(nd.shape[0]),dtypes='i1').view(numpy.recarray)
	nd=nr.append_fields(nd,'transfer',numpy.zeros(nd.shape[0]),dtypes='a1').view(numpy.recarray)

	sortedindsct = numpy.argsort(sct.id)
	sortedsctid = sct.id[sortedindsct]
	sortedindsr = numpy.argsort(sr.id)
	sortedsrid = sr.id[sortedindsr]

	for id in numpy.unique(nd.id):
		i1 = bisect.bisect_left(sortedsrid, id)
		i2 = bisect.bisect_right(sortedsrid, id)
		if (i1 != i2):
			isr=sortedindsr[i1]
			i1 = bisect.bisect_left(sortedsctid, id)
			i2 = bisect.bisect_right(sortedsctid, id)
			isct=sortedindsct[i1:i2]
			nd.gender[isct]=sr.gender[isr]
			nd.ethnicity[isct] = sr.ethnicity[isr]
			nd.transfer[isct] = sr.transfer[isr]
		if (i1 == i2):
			i1 = bisect.bisect_left(sortedsctid, id)
			i2 = bisect.bisect_right(sortedsctid, id)
			isct=sortedindsct[i1:i2]
			nd.gender[isct] = 'U'
			nd.ethnicity[isct] = -1
			nd.transfer[isct] = 'U'

	return nd
	end

# Some routines for making grade penalty plots

def make_course_grade_penalty_plot(sctall,subject,course,coursetitle=1,btelabels=0):
	nd = numpy.where((sctall.catalognbr == course) & (sctall.subject == subject))
	if len(nd[0]) == 0:
		print 'This class '+subject+' '+numpy.str(course)+' does not exist'
		return
	nd = sctall[nd[0]]

	matplotlib.pyplot.clf()
	title=nd.subject[0]+' '+str(nd.catalognbr[0])
	ax=matplotlib.pyplot.axes()
	if (coursetitle != 0):
		matplotlib.pyplot.figtext(0.5, 0.92,title,ha='center', color='black', weight='bold', size='large')
	ylim = 4.0

	#first plot the basic grade prediction from GPA
	matplotlib.pyplot.xlabel('GPA in other courses')
	matplotlib.pyplot.ylabel('Average grade')

	matplotlib.pyplot.xlim(1.,4.0)
	matplotlib.pyplot.ylim(0.,ylim)
	tdat = tprofile(nd.gpao,nd.grade,nxbins=30,xrange=[1.0,4.0])
	tpplot(tdat,color='black')
	gp = numpy.mean(nd.gpao-nd.grade)
	if (btelabels != 0):
		matplotlib.pyplot.text(1.5,3.8,'Better-Than-Expected')
		matplotlib.pyplot.text(3.0,0.8,'Worse-Than-Expected')
	matplotlib.pyplot.text(2.5,0.3,'<GPA - Grade> All = %3.2f' % (gp))
	matplotlib.pyplot.plot([0,4.],[0,4.],marker='None',ls='-',color='black')

	return

def make_course_grade_penalty_plot_by_gender(sctall,subject,course,btelabels=0):
	nd = numpy.where((sctall.subject == subject) & (sctall.catalognbr == course))
	if len(nd[0]) == 0:
		print 'This class '+subject+' '+numpy.str(course)+' does not exist'
		return
	nd = sctall[nd[0]]

	matplotlib.pyplot.clf()
	title=nd.subject[0]+' '+str(nd.catalognbr[0])
	ax=matplotlib.pyplot.axes()
	matplotlib.pyplot.figtext(0.5, 0.92,title,ha='center', color='black', weight='bold', size='large')
	ylim = 4.0

	# Now make the plot
	matplotlib.pyplot.xlim(1.,4.0)
	matplotlib.pyplot.ylim(0.,ylim)
	matplotlib.pyplot.xlabel('GPA in other courses')
	goodm = numpy.where((nd.gender == 'M'))
	goodf = numpy.where((nd.gender == 'F'))
	tdat = tprofile(nd.gpao[goodm],nd.grade[goodm],nxbins=30,xrange=[1.0,4.0])
	tpplot(tdat,noerror=1,color='r')
	tdat = tprofile(nd.gpao[goodf],nd.grade[goodf],nxbins=30,xrange=[1.0,4.0])
	tpplot(tdat,noerror=1,color='b')
	gpam = numpy.mean(nd.gpao[goodm])
	gpaf = numpy.mean(nd.gpao[goodf])
	gradem = numpy.mean(nd.grade[goodm])
	gradef = numpy.mean(nd.grade[goodf])
	gpm = gpam - gradem
	gpf = gpaf - gradef
	nmale = len(goodm[0])
	nfemale = len(goodf[0])

	if (btelabels != 0):
		matplotlib.pyplot.text(1.5,3.8,'Better-Than-Expected')
		matplotlib.pyplot.text(3.0,0.8,'Worse-Than-Expected')
	matplotlib.pyplot.text(2.75,0.90,'<GPA - Grade> Male = %3.2f' % (gpm))
	matplotlib.pyplot.text(2.75,0.75,'<GPA - Grade> Female = %3.2f' % (gpf))
	matplotlib.pyplot.text(2.75,0.6,'<GPA> Male = %3.2f' % (gpam))
	matplotlib.pyplot.text(2.75,0.45,'<GPA> Female = %3.2f' % (gpaf))
	matplotlib.pyplot.text(2.75,0.3,'<Grade> Male = %3.2f' % (gradem))
	matplotlib.pyplot.text(2.75,0.15,'<Grade> Female = %3.2f' % (gradef))
	matplotlib.pyplot.text(1.2,3.75,'Number male = %4i' %(nmale),color = 'r')
	matplotlib.pyplot.text(1.2,3.60,'Number female = %4i' %(nfemale),color = 'b')
	matplotlib.pyplot.plot([0,4.],[0,4.],marker='None',ls='-')

	return


#Some utility routines for producing grade penalty plots

def testmeans(array):

	arraymean = numpy.mean(array)
	arraystd = numpy.std(array)
	stdmean = arraystd / numpy.sqrt(len(array))

	nin = len(array)
	nhalf = nin/2

	meantest = numpy.empty(100)
	for i in range(len(meantest)):
		ar = numpy.random.uniform(size=nin)
		ind = ar.argsort()
		meantest[i] = numpy.mean(array[ind[:50]])

	return arraymean, arraystd, stdmean

def tprofile(ar1,ar2,nxbins=10,xrange=[0,0]):

	if xrange[0] == xrange[1]:
		xrange=[min(ar1),max(ar1)]

	dout=numpy.zeros(
		(nxbins),dtype=[('xmean',float),('ymean',float),('xerr',float),
		('yerr',float),('xmeanerr',float),('ymeanerr',float),
		('binstart',float),('binend',float)])

	binsize = (xrange[1] - xrange[0])/float(nxbins)
	for i in range(nxbins):
		binstart = xrange[0]+binsize*i
		binend = xrange[0]+binsize*(i+1)
		inbin = numpy.where((ar1 >= binstart) & (ar1 < binend))
		ind = inbin[0]
		dout['binstart'][i] = binstart
		dout['binend'][i] = binend
		if (len(ind) <= 2):
			print 'Too few in bin',len(ind),binstart,binend
			dout['xmean'][i] = (binend + binstart)/2.
			dout['xerr'][i] = 0.
			dout['xmeanerr'][i] = 0.
			dout['ymean'][i] = 0.
			dout['yerr'][i] = 0.
			dout['ymeanerr'][i] = 0.
		if len(ind) > 2:
			o1 = testmeans(ar1[ind])
			o2 = testmeans(ar2[ind])
			dout['xmean'][i] = o1[0]
			dout['xerr'][i] = o1[1]
			dout['xmeanerr'][i] = o1[2]
			dout['ymean'][i] = o2[0]
			dout['yerr'][i] = o2[1]
			dout['ymeanerr'][i] = o2[2]
	return dout

def tpplot(dout,noerror=0,color='k'):

	matplotlib.pyplot.plot(dout['xmean'],dout['ymean'],'+',ls='None',color=color)
	if (noerror == 0):
		matplotlib.pyplot.plot(dout['xmean'],dout['ymean']+dout['yerr'],
					marker='None',ls='--',color=color)
		matplotlib.pyplot.plot(dout['xmean'],dout['ymean']-dout['yerr'],
					marker='None',ls='--',color=color)
	matplotlib.pyplot.errorbar(dout['xmean'],dout['ymean'],xerr=dout['xmeanerr'],
						yerr=dout['ymeanerr'],ls='None',color=color)

	return

# Some routines for creating and writing out summary tables

def make_CIC_LARA_grade_penalty_table(sctall):

	courses_to_examine = numpy.where(sctall.course_code != 'NA')
	nd = sctall[courses_to_examine[0]]

	coursecodes = numpy.unique(nd.course_code)

	ct = numpy.recarray((len(coursecodes),),dtype=[ \
		('course','a10'),('ntot','i6'),('nmale','i6'),('nfemale','i6'), \
		('nyoung','i6'),('nold','i6'),('nfreshmanenter','i6'),('ntransfer','i6'), \
		('nwhite','i6'),('nhispanic','i6'),('nblack','i6'),('nasian','i6'), \
		('ntwoormore','i6'), \
		('gpa','f4'),('grade','f4'),('gpamale','f4'),('grademale','f4'), \
		('gpafemale','f4'),('gradefemale','f4'), \
		('gpayoung','f4'),('gradeyoung','f4'),('gpaold','f4'),('gradeold','f4'), \
		('gpafreshmanenter','f4'),('gradefreshmanenter','f4'),('gpatransfer','f4'),('gradetransfer','f4'),\
		('gpawhite','f4'),('gradewhite','f4'),('gpahispanic','f4'),('gradehispanic','f4'),\
		('gpablack','f4'),('gradeblack','f4'),('gpaasian','f4'),('gradeasian','f4'),
		('gpatwoormore','f4'),('gradetwoormore','f4'),])

	cn = 0
	for course in coursecodes:

		goodall = numpy.where(nd.course_code == course)
		ngoodall = len(goodall[0])
		if (ngoodall > 0):
			ct.gpa[cn] = numpy.mean(nd.gpao[goodall[0]])
			ct.grade[cn] = numpy.mean(nd.grade[goodall[0]])
		else:
			ct.gpa[cn] = 0.
			ct.grade[cn] = 0.

		goodm = numpy.where((nd.gender == 'M')& (nd.course_code == course))
		ngoodm = len(goodm[0])
		if (ngoodm > 0):
			ct.gpamale[cn] = numpy.mean(nd.gpao[goodm[0]])
			ct.grademale[cn] = numpy.mean(nd.grade[goodm[0]])
		else:
			ct.gpamale[cn] = 0.
			ct.grademale[cn] = 0.

		goodf = numpy.where((nd.gender == 'F') & (nd.course_code == course))
		ngoodf = len(goodf[0])
		if (ngoodf > 0):
			ct.gpafemale[cn] = numpy.mean(nd.gpao[goodf[0]])
			ct.gradefemale[cn] = numpy.mean(nd.grade[goodf[0]])
		else:
			ct.gpafemale[cn] = 0.
			ct.gradefemale[cn] = 0.

		incourse = numpy.where(nd.course_code == course)
		median_credits = numpy.median(nd.totalcredits[incourse[0]])
		goodyoung = numpy.where((nd.totalcredits <= median_credits) & (nd.course_code == course))
		ngoodyoung = len(goodyoung[0])
		if (ngoodyoung > 0):
			ct.gpayoung[cn] = numpy.mean(nd.gpao[goodyoung[0]])
			ct.gradeyoung[cn] = numpy.mean(nd.grade[goodyoung[0]])
		else:
			ct.gpayoung[cn] = 0.
			ct.gradeyoung[cn] = 0.

		goodold = numpy.where((nd.totalcredits > median_credits) & (nd.course_code == course))
		ngoodold = len(goodold[0])
		if (ngoodold > 0):
			ct.gpaold[cn] = numpy.mean(nd.gpao[goodold[0]])
			ct.gradeold[cn] = numpy.mean(nd.grade[goodold[0]])
		else:
			ct.gpaold[cn] = 0.
			ct.gradeold[cn] = 0.

		goodfreshmanenter = numpy.where((nd.transfer == 'N') & (nd.course_code == course))
		ngoodfreshmanenter = len(goodfreshmanenter[0])
		if (ngoodfreshmanenter > 0):
			ct.gpafreshmanenter[cn] = numpy.mean(nd.gpao[goodfreshmanenter[0]])
			ct.gradefreshmanenter[cn] = numpy.mean(nd.grade[goodfreshmanenter[0]])
		else:
			ct.gpafreshmanenter[cn] = 0.
			ct.gradefreshmanenter[cn] = 0.
		goodtransfer = numpy.where((nd.transfer == 'Y') & (nd.course_code == course))
		ngoodtransfer = len(goodtransfer[0])
		if (ngoodtransfer > 0):
			ct.gpatransfer[cn] = numpy.mean(nd.gpao[goodtransfer[0]])
			ct.gradetransfer[cn] = numpy.mean(nd.grade[goodtransfer[0]])
		else:
			ct.gpatransfer[cn] = 0.
			ct.gradetransfer[cn] = 0.

# Ethnicity codes: 1=Hispanic, 2= Native American, 3=Asian
# 4 = Black, 5=Hawaiian/Pacific, 6=White, 7=Two or more,
# 8 = Refuses to respond to both, 9=Nonresident alien

		goodwhite = numpy.where((nd.ethnicity == 6) & (nd.course_code == course))
		ngoodwhite = len(goodwhite[0])
		if (ngoodwhite > 0):
			ct.gpawhite[cn] = numpy.mean(nd.gpao[goodwhite[0]])
			ct.gradewhite[cn] = numpy.mean(nd.grade[goodwhite[0]])
		else:
			ct.gpawhite[cn] = 0.
			ct.gradewhite[cn] = 0.
		goodhispanic = numpy.where((nd.ethnicity == 1) & (nd.course_code == course))
		ngoodhispanic = len(goodhispanic[0])
		if (ngoodhispanic > 0):
			ct.gpahispanic[cn] = numpy.mean(nd.gpao[goodhispanic[0]])
			ct.gradehispanic[cn] = numpy.mean(nd.grade[goodhispanic[0]])
		else:
			ct.gpahispanic[cn] = 0.
			ct.gradehispanic[cn] = 0.
		goodblack = numpy.where((nd.ethnicity == 4) & (nd.course_code == course))
		ngoodblack = len(goodblack[0])
		if (ngoodblack > 0):
			ct.gpablack[cn] = numpy.mean(nd.gpao[goodblack[0]])
			ct.gradeblack[cn] = numpy.mean(nd.grade[goodblack[0]])
		else:
			ct.gpablack[cn] = 0.
			ct.gradeblack[cn] = 0.
		goodasian = numpy.where((nd.ethnicity == 3) & (nd.course_code == course))
		ngoodasian = len(goodasian[0])
		if (ngoodasian > 0):
			ct.gpaasian[cn] = numpy.mean(nd.gpao[goodasian[0]])
			ct.gradeasian[cn] = numpy.mean(nd.grade[goodasian[0]])
		else:
			ct.gpaasian[cn] = 0.
			ct.gradeasian[cn] = 0.
		goodtwoormore = numpy.where((nd.ethnicity == 7) & (nd.course_code == course))
		ngoodtwoormore = len(goodtwoormore[0])
		if (ngoodtwoormore > 0):
			ct.gpatwoormore[cn] = numpy.mean(nd.gpao[goodtwoormore[0]])
			ct.gradetwoormore[cn] = numpy.mean(nd.grade[goodtwoormore[0]])
		else:
			ct.gpatwoormore[cn] = 0.
			ct.gradetwoormore[cn] = 0.

		print course,ngoodall,ngoodm,ngoodf,ngoodyoung,ngoodold,ngoodfreshmanenter,ngoodtransfer
		print ngoodwhite,ngoodhispanic,ngoodblack,ngoodasian,ngoodtwoormore
		print ''

		ct.course[cn] = course
		ct.ntot[cn] = ngoodall
		ct.nmale[cn] = ngoodm
		ct.nfemale[cn] = ngoodf
		ct.nyoung[cn] = ngoodyoung
		ct.nold[cn] = ngoodold
		ct.nfreshmanenter[cn] = ngoodfreshmanenter
		ct.ntransfer[cn] = ngoodtransfer
		ct.nwhite[cn] = ngoodwhite
		ct.nhispanic[cn] = ngoodhispanic
		ct.nblack[cn] = ngoodblack
		ct.nasian[cn] = ngoodasian
		ct.ntwoormore[cn] = ngoodtwoormore

		cn = cn+1

	return ct

def make_grade_penalty_gender_difference_plot(ct,nmin=50,names=0):

	matplotlib.pyplot.clf()
	matplotlib.pyplot.xlabel('Course grade penalty')
	matplotlib.pyplot.ylabel('Male - Female grade penalty')
	matplotlib.pyplot.xlim([-0.7,0.8])
	matplotlib.pyplot.ylim([-0.4,0.3])
	good = numpy.where(ct.ntot > nmin)
	gradepen = ct.gpa[good[0]]-ct.grade[good[0]]
	gperr = 0.6/numpy.sqrt(ct.ntot[good[0]]) # errors estimated for now, will fix shortly
	gradepenmale = ct.gpamale[good[0]]-ct.grademale[good[0]]
	gradepenfemale = ct.gpafemale[good[0]]-ct.gradefemale[good[0]]
	gpdifferr = 0.6/numpy.sqrt(ct.nfemale[good[0]])
	matplotlib.pyplot.plot(gradepen,gradepenmale-gradepenfemale,'bo')
	matplotlib.pyplot.errorbar(gradepen,gradepenmale-gradepenfemale,xerr=gperr,yerr=gpdifferr,fmt=None)
	matplotlib.pyplot.plot([0,0],[-0.5,0.5],'r',ls='-')
	matplotlib.pyplot.plot([-1.0,1.0],[0,0],'r',ls='-')
	matplotlib.pyplot.plot([-1.0,1.0],[-0.1,-0.1],'b',ls=':')
	matplotlib.pyplot.plot([-1.0,1.0],[0.1,0.1],'b',ls=':')
	matplotlib.pyplot.plot([-0.1,-0.1],[-1,1],'b',ls=':')
	matplotlib.pyplot.plot([0.1,0.1],[-1,1],'b',ls=':')
	matplotlib.pyplot.text(-0.6, 0.25,'Females favored', color='red', weight='bold', size='medium')
	matplotlib.pyplot.text(-0.6, -0.3,'Males favored', color='red', weight='bold', size='medium')
	matplotlib.pyplot.text(-0.6, -0.39,'Grade bonus', color='red', weight='bold', size='medium')
	matplotlib.pyplot.text(0.4, -0.39,'Grade penalty', color='red', weight='bold', size='medium')

	if (names != 0):
		for cname in ct.course[good[0]]:
			j=numpy.where(ct.course == cname)
			if ((ct.nmale[j[0]] != 0.) & (ct.nfemale[j[0]] != 0.)):
				xt=ct.gpa[j[0]]-ct.grade[j[0]]+0.02
				yt = (ct.gpamale[j[0]]-ct.gpafemale[j[0]]-ct.grademale[j[0]]+ct.gradefemale[j[0]])-0.01
				print cname, ct.nmale[j[0]], ct.nfemale[j[0]]
				matplotlib.pyplot.text(xt[0],yt[0],cname,fontsize='8')

	return

def make_grade_penalty_freshman_transfer_plot(ct,nmin=50,names=0):

	matplotlib.pyplot.clf()
	matplotlib.pyplot.xlabel('Course grade penalty')
	matplotlib.pyplot.ylabel('Freshman - Transfer grade penalty')
	matplotlib.pyplot.xlim([-0.7,0.8])
	matplotlib.pyplot.ylim([-0.4,0.3])
	good = numpy.where(ct.ntot > nmin)
	gradepen = ct.gpa[good[0]]-ct.grade[good[0]]
	gperr = 0.6/numpy.sqrt(ct.ntot[good[0]]) # errors estimated for now, will fix shortly
	gradepenfreshmanenter = ct.gpafreshmanenter[good[0]]-ct.gradefreshmanenter[good[0]]
	gradepentransfer = ct.gpatransfer[good[0]]-ct.gradetransfer[good[0]]
	gpdifferr = 0.6/numpy.sqrt(ct.ntransfer[good[0]])
	matplotlib.pyplot.plot(gradepen,gradepenfreshmanenter-gradepentransfer,'bo')
	matplotlib.pyplot.errorbar(gradepen,gradepenfreshmanenter-gradepentransfer,xerr=gperr,yerr=gpdifferr,fmt=None)
	matplotlib.pyplot.plot([0,0],[-0.5,0.5],'r',ls='-')
	matplotlib.pyplot.plot([-1.0,1.0],[0,0],'r',ls='-')
	matplotlib.pyplot.plot([-1.0,1.0],[-0.1,-0.1],'b',ls=':')
	matplotlib.pyplot.plot([-1.0,1.0],[0.1,0.1],'b',ls=':')
	matplotlib.pyplot.plot([-0.1,-0.1],[-1,1],'b',ls=':')
	matplotlib.pyplot.plot([0.1,0.1],[-1,1],'b',ls=':')
	matplotlib.pyplot.text(-0.6, 0.25,'Transfers favored', color='red', weight='bold', size='medium')
	matplotlib.pyplot.text(-0.6, -0.3,'Freshmanenters favored', color='red', weight='bold', size='medium')
	matplotlib.pyplot.text(-0.6, -0.39,'Grade bonus', color='red', weight='bold', size='medium')
	matplotlib.pyplot.text(0.4, -0.39,'Grade penalty', color='red', weight='bold', size='medium')

	if (names != 0):
		for cname in ct.course[good[0]]:
			j=numpy.where(ct.course == cname)
			if ((ct.nfreshmanenter[j[0]] != 0.) & (ct.ntransfer[j[0]] != 0.)):
				xt=ct.gpa[j[0]]-ct.grade[j[0]]+0.02
				yt = (ct.gpafreshmanenter[j[0]]-ct.gpatransfer[j[0]]-ct.gradefreshmanenter[j[0]]+ct.gradetransfer[j[0]])-0.01
				print cname, ct.nfreshmanenter[j[0]], ct.ntransfer[j[0]]
				matplotlib.pyplot.text(xt[0],yt[0],cname,fontsize='8')
	return

student_records = load_student_record()
print "Student records loaded"
student_courses = load_student_course()
print "Student courses loaded"

print "Combining tables"
sctall = combine_sr_sct_information(student_records,student_courses)

print "Printing grade penalty table to file: grade_penalty_table.csv"
grade_penalty_table = make_CIC_LARA_grade_penalty_table(sctall)

matplotlib.mlab.rec2csv(grade_penalty_table,'grade_penalty_table.csv')
print "... Printing grade penalty table : DONE"

make_grade_penalty_gender_difference_plot(grade_penalty_table)
matplotlib.pyplot.savefig('grade_penalty_gender_difference_plot')
# make_grade_penalty_freshman_transfer_plot(grade_penalty_table)

def processCoure(plot_course_dept, plot_course_num):
	print "Plotting grade penalty"

	make_course_grade_penalty_plot(sctall,plot_course_dept,plot_course_num)
	matplotlib.pyplot.savefig('plot_'+plot_course_dept+str(plot_course_num))

	make_course_grade_penalty_plot_by_gender(sctall,plot_course_dept,plot_course_num)
	matplotlib.pyplot.savefig('plot_bygender_'+plot_course_dept+str(plot_course_num))

	print "... Plotting grade penalty : DONE"
	return

processCoure("MATH", 220)