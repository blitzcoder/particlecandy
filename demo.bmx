
Framework Openb3dmax.B3dglgraphics
Import Brl.Random

Import "particle.bmx"
Import "particletype.bmx"
Import "particlecandy.bmx"
Import "emitter.bmx"

Import "particletypes.bmx"

Import BRL.Max2D

Graphics3D 800, 600, 0, 2

Global cube:TMesh[100]

Local Light:TLight=CreateLight()
Local _cam:TCamera = CreateCamera()
CameraRange _cam, .1, 1000
CameraClsColor _cam, 64, 128, 255
PositionEntity _cam, 0, 10, 0
RotateEntity _cam, 0, -45, 0

For Local i% = 0 To 99
cube[i] = CreateCube()
PositionMesh cube[i], Rnd(100), 1, Rnd(100)
EntityColor cube[i],Rnd(255),Rnd(255),Rnd(255)
Next

PositionEntity Light,-500,-500,-500
PointEntity Light, cube[0]
MoveMouse 400 , 300

Local ground:TMesh = CreatePlane()
EntityColor ground , 0,255,0

Global _pc:ParticleCandy = New ParticleCandy.Create()
_pc.init( _cam, "particles.png", 3 )
_pc.setParticleUpdateCycle( 2 )
Global  _em0:Emitter = CreateEmitter_Test( _pc )
Global _em1:Emitter = CreateEmitter_Rocks( _pc )
Global _em2:Emitter = CreateEmitter_Explosion1( _pc )
Global _em3:Emitter = CreateEmitter_Explosion2( _pc )

HideMouse

While Not KeyDown(KEY_ESCAPE)

	UpdateWorld
	Local MSY% = MouseY() - 300
	Local MSX% = MouseX() - 400

	RotateEntity _cam, EntityPitch(_cam) + MSY/2, EntityYaw(_cam) - MSX/2, 0
	If KeyDown(KEY_S) Then MoveEntity _cam, 0, 0, -.1
	If KeyDown(KEY_W) Then MoveEntity _cam, 0, 0, .1
	If KeyDown(KEY_A)  Then MoveEntity _cam, -.1, 0, 0
	If KeyDown(KEY_D) Then MoveEntity _cam , .1 , 0 , 0

   
  	If( KeyHit( KEY_SPACE ) )
      _pc.clearParticles()
  End If    

  	If( KeyHit( KEY_1 ) )
      _pc.clearParticles()
 		 PositionEntity _em0._piv, Rnd(50), 2, Rnd(50)
      _em0.start()
  	End If

  	If( KeyHit( KEY_2 ) )		 
      If( _em0.isActive() ) _pc.clearParticles()
		 PositionEntity _em1._piv, Rnd(50), 2, Rnd(50)
      _em1.start()
  	End If

  	If( KeyHit( KEY_3 ) )
      PositionEntity _em2._piv, Rnd(50), 2, Rnd(50)
      If( _em0.isActive() ) _pc.clearParticles()
      _em2.start()
  	End If

  	If( KeyHit( KEY_4 ) )
      PositionEntity( _em3._piv, Rnd(50), 2, Rnd(50) )
      If( _em0.isActive() ) _pc.clearParticles()
      _em3.start()
  	End If
  

	MoveMouse 400 , 300
	_pc.render()
	RenderWorld

	BeginMax2D
	DrawText "Particle Candy for OpenB3DMax",0,10
	DrawText "Press 1-4 for Particly Types, Mouse for Camera Movement, Space to Clear",0,20
	EndMax2D
	
	Flip True

Wend

_pc.free()

End


