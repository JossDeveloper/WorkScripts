USE MapfreQOA
GO


/* 
----------------------------------------------------------------------------
Object:  StoredProcedure [dbo].[SPS_T_MaePersonaListado]
----------------------------------------------------------------------------
*/
IF EXISTS (SELECT 1 FROM sysobjects WHERE TYPE = 'P' AND NAME = 'SPS_T_MaePersonaListado')
	BEGIN
		DROP PROCEDURE SPS_T_MaePersonaListado
	END
	GO
	SET ANSI_NULLS ON
	GO
	SET QUOTED_IDENTIFIER ON
	GO
CREATE PROCEDURE [dbo].[SPS_T_MaePersonaListado](
    @pi_id_zonal int = 0,
    @pi_id_estado int = 0,
    @pi_id_usuario int = 0
)
AS
BEGIN
    SELECT PER.N_ID_PERSONA, PER.C_NOM_COMPLETO
    --ISNULL(PER.C_NOM_PERSONA + ' ' + PER.C_APE_PATERNO,'.') AS C_NOM_COMPLETO
    FROM T_MAE_ESTABLECIMIENTO EST WITH(NOLOCK) 
    INNER JOIN T_DET_PERSONA DPE WITH(NOLOCK) 
		ON EST.N_ID_ZONAL = @pi_id_zonal 
		AND DPE.N_ID_ESTABLECIMIENTO = EST.N_ID_ESTABLECIMIENTO 
		AND EST.N_IND_ACTIVO = 1 
		AND EST.N_ID_ESTADO = 8 
		AND DPE.N_IND_ACTIVO = 1 
		AND DPE.N_ID_ESTADO in (1, 8)
    INNER JOIN T_MAE_PERSONA PER WITH(NOLOCK)
		ON DPE.N_ID_PERSONA = PER.N_ID_PERSONA 
		AND PER.N_IND_ACTIVO = 1 
		AND PER.N_ID_ESTADO = 8
END

GO

/* 
----------------------------------------------------------------------------
Object:  StoredProcedure [dbo].[SPS_T_RepSuscripcion]
----------------------------------------------------------------------------
*/
IF EXISTS (SELECT 1 FROM sysobjects WHERE TYPE = 'P' AND NAME = 'SPS_T_RepSuscripcion')
	BEGIN
		DROP PROCEDURE SPS_T_RepSuscripcion
	END
	GO
	SET ANSI_NULLS ON
	GO
	SET QUOTED_IDENTIFIER ON
	GO
CREATE PROCEDURE  [dbo].[SPS_T_RepSuscripcion]
    @pi_id_poliza INT = 0,
    @pi_num_poliza VARCHAR(30) = '',
    @pi_num_certificado VARCHAR(30) = '',
    @pi_id_suscripcion VARCHAR(30) = '',
    @pi_id_tipoPersona SMALLINT = 0,
    @pi_id_tipoIdentidad SMALLINT = 0,
    @pi_val_docIdentidad VARCHAR(20) = '',
    @pi_apellidos VARCHAR(300) = '',
    @pi_nombres VARCHAR(300) = '',
    @pi_id_tipoFecha SMALLINT = 0,
    @pi_fec_iniVigencia VARCHAR(30) = '',
    @pi_fec_finVigencia VARCHAR(30) = '',
    @pi_id_estado SMALLINT = 0,
    @pi_id_canal INT = 0,
    @pi_id_territorio INT = 0,
    @pi_id_zona INT = 0,
    @pi_id_region INT = 0,
    @pi_id_establecimiento INT = 0,
    @pi_id_vendedor INT = 0,
    @pi_id_ramo INT = 2,
    @pi_id_producto INT = 0,
    @pi_id_plan INT = 0,
    @pi_Pagina INT = 0,
    @pi_RegxPag INT = 0,
    @pi_id_usuario INT = 0
AS
BEGIN 
	
	SET @pi_id_poliza = ISNULL(@pi_id_poliza,0)
	SET @pi_num_poliza = ISNULL(@pi_num_poliza,'')
	SET @pi_num_certificado = ISNULL(@pi_num_certificado,'')
    SET @pi_id_suscripcion = ISNULL(@pi_id_suscripcion,0)
    SET @pi_id_tipoPersona = ISNULL(@pi_id_tipoPersona,0)
	SET @pi_id_tipoIdentidad = ISNULL(@pi_id_tipoIdentidad,0)
	SET @pi_val_docIdentidad = ISNULL(@pi_val_docIdentidad,'')
	SET @pi_apellidos = ISNULL(@pi_apellidos,'')
	SET @pi_nombres = ISNULL(@pi_nombres,'')
	SET @pi_id_tipoFecha = ISNULL(@pi_id_tipoFecha,0)
	SET @pi_fec_iniVigencia = ISNULL(@pi_fec_iniVigencia,'')
	SET @pi_fec_finVigencia = ISNULL(@pi_fec_finVigencia,'')
	SET @pi_id_estado = ISNULL(@pi_id_estado,0)
	SET @pi_id_canal = ISNULL(@pi_id_canal,0)
	SET @pi_id_territorio = ISNULL(@pi_id_territorio,0)
	SET @pi_id_zona = ISNULL(@pi_id_zona,0)
	SET @pi_id_region = ISNULL(@pi_id_region,0)
	SET @pi_id_establecimiento = ISNULL(@pi_id_establecimiento,0)
    SET @pi_id_vendedor = ISNULL(@pi_id_vendedor,0)
    SET @pi_id_ramo = ISNULL(@pi_id_ramo, 2)
    SET @pi_id_producto = ISNULL(@pi_id_producto,0)
	SET @pi_id_plan = ISNULL(@pi_id_plan,0)
	

	DECLARE @v_idPersona INT
	DECLARE @v_mcaVendedor INT
	DECLARE @v_fec_iniVigencia DATETIME
	DECLARE @v_fec_finVigencia DATETIME

	DECLARE @v_Desde INT = 0
    DECLARE @v_CantidadRegistro INT = 0;
	
	SET @v_Desde = ((@pi_Pagina -1) * @pi_RegxPag + 1 )-1;
	SET @v_CantidadRegistro = @pi_RegxPag;

	IF @pi_fec_iniVigencia != ''
		SET @pi_fec_iniVigencia = SUBSTRING(@pi_fec_iniVigencia,7,4)+'-'+SUBSTRING(@pi_fec_iniVigencia,4,2)+'-'+SUBSTRING(@pi_fec_iniVigencia,1,2) + ' 00:00:00.000'

	IF @pi_fec_finVigencia != ''
		SET @pi_fec_finVigencia = SUBSTRING(@pi_fec_finVigencia,7,4)+'-'+SUBSTRING(@pi_fec_finVigencia,4,2)+'-'+SUBSTRING(@pi_fec_finVigencia,1,2) + ' 23:59:59.000'

	
    IF @pi_id_vendedor != 0
        SET @v_idPersona = @pi_id_vendedor
    ELSE 
        BEGIN
            SELECT  @v_idPersona = USU.N_ID_PERSONA, 
                    @v_mcaVendedor = PER.N_ID_MCA_VENDEDOR
            FROM T_MAE_USUARIO USU
            INNER JOIN T_DET_USUARIOPERFIL DUP ON USU.N_ID_USUARIO = DUP.N_ID_USUARIO
            INNER JOIN  T_MAE_PERFIL PER ON PER.N_ID_PERFIL = DUP.N_ID_PERFIL AND DUP.N_ID_USUARIO = @pi_id_usuario
        END
    

	DECLARE @script_ejecutar					NVARCHAR(MAX) = '',
			@condicional_certificado			VARCHAR(300) = '',
			@condicional_poliza					VARCHAR(300) = '',
			@condicional_plan					VARCHAR(300) = '',
			@condicional_producto				VARCHAR(300) = '',
			@condicional_estructura_plan		VARCHAR(MAX) = '',
			@condicional_estructura_ptoventa	VARCHAR(MAX) = '',
			@condicional_estructura_est			VARCHAR(MAX) = '',
			@condicional_ptoVenta				VARCHAR(300) = '',
			@condicional_region					VARCHAR(300) = '',
			@condicional_zona					VARCHAR(300) = '',
			@condicional_territorio				VARCHAR(300) = '',
			@condicional_tercero				VARCHAR(MAX) = ''

	--ESTRUCURA DE PRODUCTO
	IF ISNULL(@v_mcaVendedor,9) = 9
	BEGIN
		IF(SELECT COUNT(N_ID_PLAN)
		FROM T_MAE_USUARIO_CIA_PRODUCTO_PLAN
		WHERE N_ID_USUARIO = @pi_id_usuario AND N_ID_COMPANIA = 0 AND N_ID_PRODUCTO = 0 AND N_ID_PLAN = 0 AND N_ID_ESTADO = 8) = 0
		BEGIN
			IF @pi_id_producto != 0
			BEGIN
				IF @pi_id_plan != 0
				BEGIN
					SELECT @condicional_estructura_plan =  COALESCE(@condicional_estructura_plan,'') + CAST(N_ID_PLAN AS VARCHAR(10)) + ',' FROM 
					(SELECT PL.N_ID_PLAN 
					FROM T_MAE_PLAN PL
					INNER JOIN T_MAE_PRODUCTO PRD ON PL.N_ID_PRODUCTO = PRD.N_ID_PRODUCTO
					INNER JOIN T_MAE_USUARIO_CIA_PRODUCTO_PLAN CPP ON CPP.N_ID_USUARIO = @pi_id_usuario
						AND CPP.N_ID_COMPANIA = PRD.N_ID_ENTIDAD 
						AND (CPP.N_ID_PRODUCTO = 0 OR CPP.N_ID_PRODUCTO = PRD.N_ID_PRODUCTO)
						AND (CPP.N_ID_PLAN = 0 OR CPP.N_ID_PLAN = PL.N_ID_PLAN)
						AND CPP.N_ID_ESTADO = 8
					WHERE PL.N_ID_PLAN = @pi_id_plan) A
				END
				ELSE
				BEGIN
					SELECT @condicional_estructura_plan =  COALESCE(@condicional_estructura_plan,'') + CAST(N_ID_PLAN AS VARCHAR(10)) + ',' FROM 
					(SELECT PL.N_ID_PLAN 
					FROM T_MAE_PLAN PL
					INNER JOIN T_MAE_PRODUCTO PRD ON PL.N_ID_PRODUCTO = PRD.N_ID_PRODUCTO
					INNER JOIN T_MAE_USUARIO_CIA_PRODUCTO_PLAN CPP ON CPP.N_ID_USUARIO = @pi_id_usuario
						AND CPP.N_ID_COMPANIA = PRD.N_ID_ENTIDAD 
						AND (CPP.N_ID_PRODUCTO = 0 OR CPP.N_ID_PRODUCTO = PRD.N_ID_PRODUCTO)
						AND (CPP.N_ID_PLAN = 0 OR CPP.N_ID_PLAN = PL.N_ID_PLAN)
						AND CPP.N_ID_ESTADO = 8
					WHERE PRD.N_ID_PRODUCTO = @pi_id_producto) A
				END
			END
			ELSE
			BEGIN
				SELECT @condicional_estructura_plan =  COALESCE(@condicional_estructura_plan,'') + CAST(N_ID_PLAN AS VARCHAR(10)) + ',' FROM 
				(SELECT PL.N_ID_PLAN 
				FROM T_MAE_PLAN PL
				INNER JOIN T_MAE_PRODUCTO PRD ON PL.N_ID_PRODUCTO = PRD.N_ID_PRODUCTO
				INNER JOIN T_MAE_USUARIO_CIA_PRODUCTO_PLAN CPP ON CPP.N_ID_USUARIO = @pi_id_usuario
					AND CPP.N_ID_COMPANIA = PRD.N_ID_ENTIDAD 
					AND (CPP.N_ID_PRODUCTO = 0 OR CPP.N_ID_PRODUCTO = PRD.N_ID_PRODUCTO)
					AND (CPP.N_ID_PLAN = 0 OR CPP.N_ID_PLAN = PL.N_ID_PLAN)
					AND CPP.N_ID_ESTADO = 8) A
			END

			IF LEN(@condicional_estructura_plan) > 0 
			BEGIN	
				SET @condicional_estructura_plan = SUBSTRING(@condicional_estructura_plan,1,LEN(@condicional_estructura_plan)-1)	
				SET @condicional_estructura_plan = ' AND POL.N_ID_PLAN IN (' + @condicional_estructura_plan + ')'	
			END	
			ELSE
			BEGIN
				SET @condicional_estructura_plan = ' AND POL.N_ID_PLAN IN (0)'	
			END
		END
		ELSE
		BEGIN
            IF @pi_id_producto != 0
            BEGIN
                IF @pi_id_plan != 0
                BEGIN
                    SET @condicional_poliza = @condicional_poliza + ' AND POL.N_ID_PLAN = ' + CONVERT(VARCHAR(10),@pi_id_plan)
                END
                ELSE
                BEGIN
                    SET @condicional_plan = @condicional_plan + ' AND PL.N_ID_PRODUCTO = '  + CONVERT(VARCHAR(10),@pi_id_producto)
                END
            END
		END
	END
	ELSE
	BEGIN
        IF @pi_id_producto != 0
        BEGIN
            IF @pi_id_plan != 0
                SET @condicional_poliza = @condicional_poliza + ' AND POL.N_ID_PLAN = ' + CONVERT(VARCHAR(10),@pi_id_plan)
            ELSE
                SET @condicional_plan = @condicional_plan + ' AND PL.N_ID_PRODUCTO = '  + CONVERT(VARCHAR(10),@pi_id_producto)
        END

		SET @condicional_plan = @condicional_plan + ' AND CER.N_ID_PERVENDEDOR = ' + CONVERT(VARCHAR,@v_idPersona)
	END

	--ESTRUCTURA COMERCIAL
	IF ISNULL(@v_mcaVendedor,9) = 9
	BEGIN
		IF(SELECT COUNT(N_ID_PUNTO_VENTA) AS V_VENTA
		FROM T_MAE_USUARIO_CANAL_PTOVENTA
		WHERE N_ID_USUARIO = @pi_id_usuario AND N_ID_CANAL = 0 AND N_ID_TERRITORIO = 0 AND N_ID_ZONA = 0 AND N_ID_REGION = 0 AND N_ID_PUNTO_VENTA = 0 AND N_ID_ESTADO = 8) = 0
		BEGIN
			IF @pi_id_canal != 0
			BEGIN
				IF @pi_id_territorio != 0
				BEGIN
					IF @pi_id_zona != 0
					BEGIN
						IF @pi_id_region != 0
						BEGIN
							IF @pi_id_establecimiento !=0
							BEGIN
								SELECT @condicional_estructura_est =  COALESCE(@condicional_estructura_est,'') + CAST(N_ID_ESTABLECIMIENTO AS VARCHAR(10)) + ',' FROM 
								(SELECT PVT.N_ID_ESTABLECIMIENTO
								FROM T_MAE_ZONAL PVT
								JOIN T_MAE_ZONAL REG ON PVT.N_ID_ZONALPADRE = REG.N_ID_ZONAL AND PVT.N_ID_TIPOZONAL = 406 AND REG.N_ID_TIPOZONAL = 405
								JOIN T_MAE_ZONAL ZON ON REG.N_ID_ZONALPADRE = ZON.N_ID_ZONAL AND ZON.N_ID_TIPOZONAL = 404
								JOIN T_MAE_ZONAL TER ON ZON.N_ID_ZONALPADRE = TER.N_ID_ZONAL AND TER.N_ID_TIPOZONAL = 403
								JOIN T_MAE_USUARIO_CANAL_PTOVENTA CTZRP ON CTZRP.N_ID_USUARIO = @pi_id_usuario
																		AND CTZRP.N_ID_CANAL = TER.N_ID_ENTIDAD
																		AND (CTZRP.N_ID_TERRITORIO = 0 OR CTZRP.N_ID_TERRITORIO = TER.N_ID_ZONAL)
																		AND (CTZRP.N_ID_ZONA = 0 OR CTZRP.N_ID_ZONA = ZON.N_ID_ZONAL)
																		AND (CTZRP.N_ID_REGION = 0 OR CTZRP.N_ID_REGION = REG.N_ID_ZONAL)
																		AND (CTZRP.N_ID_PUNTO_VENTA = 0 OR CTZRP.N_ID_PUNTO_VENTA = PVT.N_ID_ZONAL)
																		AND CTZRP.N_ID_ESTADO = 8
								WHERE TER.N_ID_ENTIDAD = @pi_id_canal AND TER.N_ID_ZONAL = @pi_id_territorio AND ZON.N_ID_ZONAL = @pi_id_zona AND REG.N_ID_ZONAL = @pi_id_region AND PVT.N_ID_ZONAL = @pi_id_establecimiento) A
							END
							ELSE
							BEGIN
								SELECT @condicional_estructura_est =  COALESCE(@condicional_estructura_est,'') + CAST(N_ID_ESTABLECIMIENTO AS VARCHAR(10)) + ',' FROM 
								(SELECT PVT.N_ID_ESTABLECIMIENTO
								FROM T_MAE_ZONAL PVT
								JOIN T_MAE_ZONAL REG ON PVT.N_ID_ZONALPADRE = REG.N_ID_ZONAL AND PVT.N_ID_TIPOZONAL = 406 AND REG.N_ID_TIPOZONAL = 405
								JOIN T_MAE_ZONAL ZON ON REG.N_ID_ZONALPADRE = ZON.N_ID_ZONAL AND ZON.N_ID_TIPOZONAL = 404
								JOIN T_MAE_ZONAL TER ON ZON.N_ID_ZONALPADRE = TER.N_ID_ZONAL AND TER.N_ID_TIPOZONAL = 403
								JOIN T_MAE_USUARIO_CANAL_PTOVENTA CTZRP ON CTZRP.N_ID_USUARIO = @pi_id_usuario
																		AND CTZRP.N_ID_CANAL = TER.N_ID_ENTIDAD
																		AND (CTZRP.N_ID_TERRITORIO = 0 OR CTZRP.N_ID_TERRITORIO = TER.N_ID_ZONAL)
																		AND (CTZRP.N_ID_ZONA = 0 OR CTZRP.N_ID_ZONA = ZON.N_ID_ZONAL)
																		AND (CTZRP.N_ID_REGION = 0 OR CTZRP.N_ID_REGION = REG.N_ID_ZONAL)
																		AND (CTZRP.N_ID_PUNTO_VENTA = 0 OR CTZRP.N_ID_PUNTO_VENTA = PVT.N_ID_ZONAL)
																		AND CTZRP.N_ID_ESTADO = 8
								WHERE TER.N_ID_ENTIDAD = @pi_id_canal AND TER.N_ID_ZONAL = @pi_id_territorio AND ZON.N_ID_ZONAL = @pi_id_zona AND REG.N_ID_ZONAL = @pi_id_region) A
							END
						END
						ELSE
						BEGIN
							SELECT @condicional_estructura_est =  COALESCE(@condicional_estructura_est,'') + CAST(N_ID_ESTABLECIMIENTO AS VARCHAR(10)) + ',' FROM 
							(SELECT PVT.N_ID_ESTABLECIMIENTO
							FROM T_MAE_ZONAL PVT
							JOIN T_MAE_ZONAL REG ON PVT.N_ID_ZONALPADRE = REG.N_ID_ZONAL AND PVT.N_ID_TIPOZONAL = 406 AND REG.N_ID_TIPOZONAL = 405
							JOIN T_MAE_ZONAL ZON ON REG.N_ID_ZONALPADRE = ZON.N_ID_ZONAL AND ZON.N_ID_TIPOZONAL = 404
							JOIN T_MAE_ZONAL TER ON ZON.N_ID_ZONALPADRE = TER.N_ID_ZONAL AND TER.N_ID_TIPOZONAL = 403
							JOIN T_MAE_USUARIO_CANAL_PTOVENTA CTZRP ON CTZRP.N_ID_USUARIO = @pi_id_usuario
																	AND CTZRP.N_ID_CANAL = TER.N_ID_ENTIDAD
																	AND (CTZRP.N_ID_TERRITORIO = 0 OR CTZRP.N_ID_TERRITORIO = TER.N_ID_ZONAL)
																	AND (CTZRP.N_ID_ZONA = 0 OR CTZRP.N_ID_ZONA = ZON.N_ID_ZONAL)
																	AND (CTZRP.N_ID_REGION = 0 OR CTZRP.N_ID_REGION = REG.N_ID_ZONAL)
																	AND (CTZRP.N_ID_PUNTO_VENTA = 0 OR CTZRP.N_ID_PUNTO_VENTA = PVT.N_ID_ZONAL)
																	AND CTZRP.N_ID_ESTADO = 8
							WHERE TER.N_ID_ENTIDAD = @pi_id_canal AND TER.N_ID_ZONAL = @pi_id_territorio AND ZON.N_ID_ZONAL = @pi_id_zona) A
						END
					END
					ELSE
					BEGIN
						SELECT @condicional_estructura_est =  COALESCE(@condicional_estructura_est,'') + CAST(N_ID_ESTABLECIMIENTO AS VARCHAR(10)) + ',' FROM 
						(SELECT PVT.N_ID_ESTABLECIMIENTO
						FROM T_MAE_ZONAL PVT
						JOIN T_MAE_ZONAL REG ON PVT.N_ID_ZONALPADRE = REG.N_ID_ZONAL AND PVT.N_ID_TIPOZONAL = 406 AND REG.N_ID_TIPOZONAL = 405
						JOIN T_MAE_ZONAL ZON ON REG.N_ID_ZONALPADRE = ZON.N_ID_ZONAL AND ZON.N_ID_TIPOZONAL = 404
						JOIN T_MAE_ZONAL TER ON ZON.N_ID_ZONALPADRE = TER.N_ID_ZONAL AND TER.N_ID_TIPOZONAL = 403
						JOIN T_MAE_USUARIO_CANAL_PTOVENTA CTZRP ON CTZRP.N_ID_USUARIO = @pi_id_usuario
																AND CTZRP.N_ID_CANAL = TER.N_ID_ENTIDAD
																AND (CTZRP.N_ID_TERRITORIO = 0 OR CTZRP.N_ID_TERRITORIO = TER.N_ID_ZONAL)
																AND (CTZRP.N_ID_ZONA = 0 OR CTZRP.N_ID_ZONA = ZON.N_ID_ZONAL)
																AND (CTZRP.N_ID_REGION = 0 OR CTZRP.N_ID_REGION = REG.N_ID_ZONAL)
																AND (CTZRP.N_ID_PUNTO_VENTA = 0 OR CTZRP.N_ID_PUNTO_VENTA = PVT.N_ID_ZONAL)
																AND CTZRP.N_ID_ESTADO = 8
						WHERE TER.N_ID_ENTIDAD = @pi_id_canal AND TER.N_ID_ZONAL = @pi_id_territorio) A
					END
				END
				ELSE
				BEGIN
					SELECT @condicional_estructura_est =  COALESCE(@condicional_estructura_est,'') + CAST(N_ID_ESTABLECIMIENTO AS VARCHAR(10)) + ',' FROM 
					(SELECT PVT.N_ID_ESTABLECIMIENTO
					FROM T_MAE_ZONAL PVT
					JOIN T_MAE_ZONAL REG ON PVT.N_ID_ZONALPADRE = REG.N_ID_ZONAL AND PVT.N_ID_TIPOZONAL = 406 AND REG.N_ID_TIPOZONAL = 405
					JOIN T_MAE_ZONAL ZON ON REG.N_ID_ZONALPADRE = ZON.N_ID_ZONAL AND ZON.N_ID_TIPOZONAL = 404
					JOIN T_MAE_ZONAL TER ON ZON.N_ID_ZONALPADRE = TER.N_ID_ZONAL AND TER.N_ID_TIPOZONAL = 403
					JOIN T_MAE_USUARIO_CANAL_PTOVENTA CTZRP ON CTZRP.N_ID_USUARIO = @pi_id_usuario
															AND CTZRP.N_ID_CANAL = TER.N_ID_ENTIDAD
															AND (CTZRP.N_ID_TERRITORIO = 0 OR CTZRP.N_ID_TERRITORIO = TER.N_ID_ZONAL)
															AND (CTZRP.N_ID_ZONA = 0 OR CTZRP.N_ID_ZONA = ZON.N_ID_ZONAL)
															AND (CTZRP.N_ID_REGION = 0 OR CTZRP.N_ID_REGION = REG.N_ID_ZONAL)
															AND (CTZRP.N_ID_PUNTO_VENTA = 0 OR CTZRP.N_ID_PUNTO_VENTA = PVT.N_ID_ZONAL)
															AND CTZRP.N_ID_ESTADO = 8
					WHERE TER.N_ID_ENTIDAD = @pi_id_canal) A
				END
			END
			ELSE
			BEGIN
				SELECT @condicional_estructura_est =  COALESCE(@condicional_estructura_est,'') + CAST(N_ID_ESTABLECIMIENTO AS VARCHAR(10)) + ',' FROM 
				(SELECT PVT.N_ID_ESTABLECIMIENTO
				FROM T_MAE_ZONAL PVT
				JOIN T_MAE_ZONAL REG ON PVT.N_ID_ZONALPADRE = REG.N_ID_ZONAL AND PVT.N_ID_TIPOZONAL = 406 AND REG.N_ID_TIPOZONAL = 405
				JOIN T_MAE_ZONAL ZON ON REG.N_ID_ZONALPADRE = ZON.N_ID_ZONAL AND ZON.N_ID_TIPOZONAL = 404
				JOIN T_MAE_ZONAL TER ON ZON.N_ID_ZONALPADRE = TER.N_ID_ZONAL AND TER.N_ID_TIPOZONAL = 403
				JOIN T_MAE_USUARIO_CANAL_PTOVENTA CTZRP ON CTZRP.N_ID_USUARIO = @pi_id_usuario
					AND CTZRP.N_ID_CANAL = TER.N_ID_ENTIDAD
					AND (CTZRP.N_ID_TERRITORIO = 0 OR CTZRP.N_ID_TERRITORIO = TER.N_ID_ZONAL)
					AND (CTZRP.N_ID_ZONA = 0 OR CTZRP.N_ID_ZONA = ZON.N_ID_ZONAL)
					AND (CTZRP.N_ID_REGION = 0 OR CTZRP.N_ID_REGION = REG.N_ID_ZONAL)
					AND (CTZRP.N_ID_PUNTO_VENTA = 0 OR CTZRP.N_ID_PUNTO_VENTA = PVT.N_ID_ZONAL)
					 AND CTZRP.N_ID_ESTADO = 8) A
			END	

			IF LEN(@condicional_estructura_est) > 0 
			BEGIN	
				SET @condicional_estructura_est = SUBSTRING(@condicional_estructura_est,1,LEN(@condicional_estructura_est)-1)	
				SET @condicional_estructura_est = ' AND CER.N_ID_ESTABLECIMIENTO IN (' + @condicional_estructura_est + ')'	
			END	
			ELSE
			BEGIN
				SET @condicional_estructura_est = ' AND CER.N_ID_ESTABLECIMIENTO IN (0)'	
			END
		END
		ELSE
		BEGIN
			IF @pi_id_canal != 0
			BEGIN
				IF @pi_id_territorio != 0
				BEGIN
					IF @pi_id_zona != 0
					BEGIN
						IF @pi_id_region != 0
						BEGIN
							IF @pi_id_establecimiento != 0
							BEGIN
								SELECT @condicional_estructura_est =  COALESCE(@condicional_estructura_est,'') + CAST(N_ID_ESTABLECIMIENTO AS VARCHAR(10)) + ',' FROM 
								(SELECT PVT.N_ID_ESTABLECIMIENTO
								FROM T_MAE_ZONAL PVT
								WHERE PVT.N_ID_ZONAL = @pi_id_establecimiento) A
							END
							ELSE
							BEGIN
								SELECT @condicional_estructura_est =  COALESCE(@condicional_estructura_est,'') + CAST(N_ID_ESTABLECIMIENTO AS VARCHAR(10)) + ',' FROM 
								(SELECT PVT.N_ID_ESTABLECIMIENTO
								FROM T_MAE_ZONAL PVT
								JOIN T_MAE_ZONAL REG ON PVT.N_ID_ZONALPADRE = REG.N_ID_ZONAL AND PVT.N_ID_TIPOZONAL = 406 AND REG.N_ID_TIPOZONAL = 405
								WHERE PVT.N_ID_ZONALPADRE = @pi_id_region) A
							END
						END
						ELSE
						BEGIN
							SELECT @condicional_estructura_est =  COALESCE(@condicional_estructura_est,'') + CAST(N_ID_ESTABLECIMIENTO AS VARCHAR(10)) + ',' FROM 
							(SELECT PVT.N_ID_ESTABLECIMIENTO
							FROM T_MAE_ZONAL PVT
							JOIN T_MAE_ZONAL REG ON PVT.N_ID_ZONALPADRE = REG.N_ID_ZONAL AND PVT.N_ID_TIPOZONAL = 406 AND REG.N_ID_TIPOZONAL = 405
							JOIN T_MAE_ZONAL ZON ON REG.N_ID_ZONALPADRE = ZON.N_ID_ZONAL AND ZON.N_ID_TIPOZONAL = 404
							WHERE REG.N_ID_ZONALPADRE = @pi_id_zona) A
						END
					END
					ELSE
					BEGIN
						SELECT @condicional_estructura_est =  COALESCE(@condicional_estructura_est,'') + CAST(N_ID_ESTABLECIMIENTO AS VARCHAR(10)) + ',' FROM 
						(SELECT PVT.N_ID_ESTABLECIMIENTO
						FROM T_MAE_ZONAL PVT
						JOIN T_MAE_ZONAL REG ON PVT.N_ID_ZONALPADRE = REG.N_ID_ZONAL AND PVT.N_ID_TIPOZONAL = 406 AND REG.N_ID_TIPOZONAL = 405
						JOIN T_MAE_ZONAL ZON ON REG.N_ID_ZONALPADRE = ZON.N_ID_ZONAL AND ZON.N_ID_TIPOZONAL = 404
						JOIN T_MAE_ZONAL TER ON ZON.N_ID_ZONALPADRE = TER.N_ID_ZONAL AND TER.N_ID_TIPOZONAL = 403
						WHERE ZON.N_ID_ZONALPADRE = @pi_id_territorio) A
					END
				END
				ELSE
				BEGIN
					SELECT @condicional_estructura_est =  COALESCE(@condicional_estructura_est,'') + CAST(N_ID_ESTABLECIMIENTO AS VARCHAR(10)) + ',' FROM 
					(SELECT PVT.N_ID_ESTABLECIMIENTO
					FROM T_MAE_ZONAL PVT
					JOIN T_MAE_ZONAL REG ON PVT.N_ID_ZONALPADRE = REG.N_ID_ZONAL AND PVT.N_ID_TIPOZONAL = 406 AND REG.N_ID_TIPOZONAL = 405
					JOIN T_MAE_ZONAL ZON ON REG.N_ID_ZONALPADRE = ZON.N_ID_ZONAL AND ZON.N_ID_TIPOZONAL = 404
					JOIN T_MAE_ZONAL TER ON ZON.N_ID_ZONALPADRE = TER.N_ID_ZONAL AND TER.N_ID_TIPOZONAL = 403
					WHERE ZON.N_ID_ENTIDAD = @pi_id_canal) A
				END
			END

			IF LEN(@condicional_estructura_est) > 0 
			BEGIN	
				SET @condicional_estructura_est = SUBSTRING(@condicional_estructura_est,1,LEN(@condicional_estructura_est)-1)	
				SET @condicional_estructura_est = ' AND CER.N_ID_ESTABLECIMIENTO IN (' + @condicional_estructura_est + ')'	
			END	
		END
	END
	ELSE
	BEGIN
		IF @pi_id_canal != 0
		BEGIN
			IF @pi_id_territorio != 0
			BEGIN
				IF @pi_id_zona != 0
				BEGIN
					IF @pi_id_region != 0
					BEGIN
						IF @pi_id_establecimiento != 0
						BEGIN
							SELECT @condicional_estructura_est =  COALESCE(@condicional_estructura_est,'') + CAST(N_ID_ESTABLECIMIENTO AS VARCHAR(10)) + ',' FROM 
							(SELECT PVT.N_ID_ESTABLECIMIENTO
							FROM T_MAE_ZONAL PVT
							WHERE PVT.N_ID_ZONAL = @pi_id_establecimiento) A
						END
						ELSE
						BEGIN
							SELECT @condicional_estructura_est =  COALESCE(@condicional_estructura_est,'') + CAST(N_ID_ESTABLECIMIENTO AS VARCHAR(10)) + ',' FROM 
							(SELECT PVT.N_ID_ESTABLECIMIENTO
							FROM T_MAE_ZONAL PVT
							JOIN T_MAE_ZONAL REG ON PVT.N_ID_ZONALPADRE = REG.N_ID_ZONAL AND PVT.N_ID_TIPOZONAL = 406 AND REG.N_ID_TIPOZONAL = 405
							WHERE PVT.N_ID_ZONALPADRE = @pi_id_region) A
						END
					END
					ELSE
					BEGIN
						SELECT @condicional_estructura_est =  COALESCE(@condicional_estructura_est,'') + CAST(N_ID_ESTABLECIMIENTO AS VARCHAR(10)) + ',' FROM 
							(SELECT PVT.N_ID_ESTABLECIMIENTO
							FROM T_MAE_ZONAL PVT
							JOIN T_MAE_ZONAL REG ON PVT.N_ID_ZONALPADRE = REG.N_ID_ZONAL AND PVT.N_ID_TIPOZONAL = 406 AND REG.N_ID_TIPOZONAL = 405
							JOIN T_MAE_ZONAL ZON ON REG.N_ID_ZONALPADRE = ZON.N_ID_ZONAL AND ZON.N_ID_TIPOZONAL = 404
							WHERE REG.N_ID_ZONALPADRE = @pi_id_zona) A
					END
				END
				ELSE
				BEGIN
					SELECT @condicional_estructura_est =  COALESCE(@condicional_estructura_est,'') + CAST(N_ID_ESTABLECIMIENTO AS VARCHAR(10)) + ',' FROM 
						(SELECT PVT.N_ID_ESTABLECIMIENTO
						FROM T_MAE_ZONAL PVT
						JOIN T_MAE_ZONAL REG ON PVT.N_ID_ZONALPADRE = REG.N_ID_ZONAL AND PVT.N_ID_TIPOZONAL = 406 AND REG.N_ID_TIPOZONAL = 405
						JOIN T_MAE_ZONAL ZON ON REG.N_ID_ZONALPADRE = ZON.N_ID_ZONAL AND ZON.N_ID_TIPOZONAL = 404
						JOIN T_MAE_ZONAL TER ON ZON.N_ID_ZONALPADRE = TER.N_ID_ZONAL AND TER.N_ID_TIPOZONAL = 403
						WHERE ZON.N_ID_ZONALPADRE = @pi_id_territorio) A
				END
			END
			ELSE
			BEGIN
				SELECT @condicional_estructura_est =  COALESCE(@condicional_estructura_est,'') + CAST(N_ID_ESTABLECIMIENTO AS VARCHAR(10)) + ',' FROM 
					(SELECT PVT.N_ID_ESTABLECIMIENTO
					FROM T_MAE_ZONAL PVT
					JOIN T_MAE_ZONAL REG ON PVT.N_ID_ZONALPADRE = REG.N_ID_ZONAL AND PVT.N_ID_TIPOZONAL = 406 AND REG.N_ID_TIPOZONAL = 405
					JOIN T_MAE_ZONAL ZON ON REG.N_ID_ZONALPADRE = ZON.N_ID_ZONAL AND ZON.N_ID_TIPOZONAL = 404
					JOIN T_MAE_ZONAL TER ON ZON.N_ID_ZONALPADRE = TER.N_ID_ZONAL AND TER.N_ID_TIPOZONAL = 403
					WHERE ZON.N_ID_ENTIDAD = @pi_id_canal) A
			END
		END

		IF LEN(@condicional_estructura_est) > 0 
		BEGIN	
			SET @condicional_estructura_est = SUBSTRING(@condicional_estructura_est,1,LEN(@condicional_estructura_est)-1)	
			SET @condicional_estructura_est = ' AND CER.N_ID_ESTABLECIMIENTO IN (' + @condicional_estructura_est + ')'	
		END	
	END

	IF @pi_id_estado = 0
		SET @condicional_certificado = @condicional_certificado + ' AND CER.N_ID_ESTADO IN (4,7,15,18,25,26,27)'
	ELSE 
		SET @condicional_certificado = @condicional_certificado + ' AND CER.N_ID_ESTADO = ' + CONVERT(VARCHAR(5),@pi_id_estado)
	
	IF @pi_id_tipoFecha = 426
	BEGIN
		IF @pi_fec_iniVigencia != ''
			SET @condicional_certificado = @condicional_certificado + ' AND CER.D_FEC_VENTA >= ''' + @pi_fec_iniVigencia + ''''

		IF @pi_fec_finVigencia != ''
			SET @condicional_certificado = @condicional_certificado + ' AND CER.D_FEC_VENTA <= ''' + @pi_fec_finVigencia + ''''		
	END

	IF @pi_id_tipoFecha = 427
	BEGIN
		IF @pi_fec_iniVigencia != ''
			SET @condicional_certificado = @condicional_certificado + ' AND CER.D_FEC_ANULACION >= ''' + @pi_fec_iniVigencia + ''''

		IF @pi_fec_finVigencia != ''
			SET @condicional_certificado = @condicional_certificado + ' AND CER.D_FEC_ANULACION <= ''' + @pi_fec_finVigencia + ''''		
	END

	IF @pi_id_tipoFecha = 585
	BEGIN
		IF @pi_fec_iniVigencia != ''
			SET @condicional_certificado = @condicional_certificado + ' AND CER.D_FEC_FINVIGENCIA >= ''' + @pi_fec_iniVigencia + ''''

		IF @pi_fec_finVigencia != ''
			SET @condicional_certificado = @condicional_certificado + ' AND CER.D_FEC_FINVIGENCIA <= ''' + @pi_fec_finVigencia + ''''		
	END

	IF @pi_id_tipoFecha = 586
	BEGIN
		IF @pi_fec_iniVigencia != ''
			SET @condicional_certificado = @condicional_certificado + ' AND CER.D_FEC_INIVIGENCIA >= ''' + @pi_fec_iniVigencia + ''''

		IF @pi_fec_finVigencia != ''
			SET @condicional_certificado = @condicional_certificado + ' AND CER.D_FEC_INIVIGENCIA <= ''' + @pi_fec_finVigencia + ''''		
	END
	
	IF @pi_id_tipoFecha = 596
	BEGIN
		IF @pi_fec_iniVigencia != ''
			SET @condicional_certificado = @condicional_certificado + ' AND CER.D_FEC_CERTIFICADO >= ''' + @pi_fec_iniVigencia + ''''

		IF @pi_fec_finVigencia != ''
			SET @condicional_certificado = @condicional_certificado + ' AND CER.D_FEC_CERTIFICADO <= ''' + @pi_fec_finVigencia + ''''		
	END

	IF @pi_num_certificado !=''
		SET @condicional_certificado = @condicional_certificado + ' AND CER.C_NUM_CERTIFICADO =''' + @pi_num_certificado + ''''
    
    IF @pi_id_suscripcion != 0
        SET @condicional_certificado = @condicional_certificado + ' AND CER.N_ID_SUSCRIPCION = ' + CONVERT(VARCHAR(10),@pi_id_suscripcion)
    
    IF @pi_id_poliza != 0
		SET @condicional_poliza = @condicional_poliza + ' AND POL.N_ID_POLIZA = ' + CONVERT(VARCHAR(10),@pi_id_poliza)
    
    IF @pi_num_poliza != ''
		SET @condicional_poliza = @condicional_poliza + ' AND POL.C_NUM_POLIZA = ''' + CONVERT(VARCHAR(10),@pi_num_poliza) + ''''
    
    IF @pi_id_ramo != 0
        SET @condicional_producto = @condicional_producto + ' AND PR.N_ID_RAMO = ' + + CONVERT(VARCHAR(10),@pi_id_ramo)
    

	
	--@condicional_tercero

	IF @pi_id_tipoPersona != 0
	BEGIN
		DECLARE @CADENA	VARCHAR(MAX) = ''
		DECLARE @WHERE VARCHAR(MAX) = ''
		DECLARE @TABLA_X TABLE(CADENA1	VARCHAR(MAX)) 
		DECLARE @N_ID_CERTIFICADO VARCHAR(MAX)

		IF @pi_id_tipoIdentidad != 0 AND @pi_val_docIdentidad != ''
			SET @WHERE = @WHERE + ' AND TER.N_ID_TIPOIDENTIDAD = ' + CONVERT(VARCHAR(5),@pi_id_tipoIdentidad) + ' AND TER.C_VAL_NUMIDENTIDAD =''' + @pi_val_docIdentidad + ''''
	
		IF @pi_apellidos != ''
			SET @WHERE = @WHERE + ' AND (TER.C_APE_PATERNO +'' ''+ TER.C_APE_MATERNO) LIKE ''%' + @pi_apellidos +'%'''

		IF @pi_nombres != ''
			SET @WHERE = @WHERE + ' AND TER.C_NOM_TERCERO LIKE ''%' + @pi_nombres +'%'''

		SET @CADENA = '
		DECLARE @A1	VARCHAR(MAX)

		SELECT @A1 = COALESCE(@A1,'''') + CAST(N_ID_CERTIFICADO AS VARCHAR(10)) + '','' FROM (
			SELECT RT.N_ID_CERTIFICADO
			FROM T_MAE_TERCERO TER
			INNER JOIN T_DET_ROLTERCEROCERTIFICADO RT ON TER.N_ID_TERCERO = RT.N_ID_TERCERO AND RT.N_ID_TIPOROLTERCERO = ' + CONVERT(VARCHAR,@pi_id_tipoPersona) + ' AND RT.N_ID_ESTADO = 8 AND RT.N_IND_ACTIVO = 1 '
			+ @WHERE + ') A
	
		SELECT @A1
		'

		INSERT @TABLA_X
		exec (@CADENA)

		SET @condicional_tercero = (SELECT CADENA1 FROM @TABLA_X)

		IF LEN(@condicional_tercero) > 0 
		BEGIN	
			SET @condicional_tercero = SUBSTRING(@condicional_tercero,1,LEN(@condicional_tercero)-1)	
			SET @condicional_tercero = ' AND CER.N_ID_CERTIFICADO IN (' + @condicional_tercero + ')'	
		END	
	END

	SET @script_ejecutar = '
		WITH TABLA_TOTAL AS (
			SELECT COUNT(1) AS CANTIDAD
			FROM T_MAE_CERTIFICADO CER  
		INNER JOIN T_MAE_POLIZA POL ON CER.N_IND_ACTIVO = 1' + @condicional_certificado + @condicional_tercero + ' AND CER.N_ID_POLIZA = POL.N_ID_POLIZA' 
												+ @condicional_poliza + @condicional_estructura_plan + @condicional_estructura_est +
		' INNER JOIN T_MAE_PLAN PL ON POL.N_ID_PLAN = PL.N_ID_PLAN' + @condicional_plan +
		' INNER JOIN T_MAE_PRODUCTO PR ON PL.N_ID_PRODUCTO = PR.N_ID_PRODUCTO' + @condicional_producto + 
		' INNER JOIN T_MAE_ENTIDAD ENT ON PR.N_ID_ENTIDAD = ENT.N_ID_ENTIDAD)
		,TABLA_1 AS (
		SELECT 
			ROW_NUMBER() OVER (ORDER BY CER.N_ID_CERTIFICADO DESC)	AS NUMROW,
			(SELECT CANTIDAD FROM TABLA_TOTAL) AS ROW_TOTAL,
			CER.N_ID_POLIZA,
			POL.C_NUM_POLIZA,
			CER.N_ID_CERTIFICADO,
			CER.C_NUM_CERTIFICADO,
			CER.N_ID_COTIZACION,
            CER.N_ID_SUSCRIPCION,
			CER.C_NUM_SOLICITUD,
            CER.C_NUM_CREDITO,
			ENT.N_ID_ENTIDAD,
			ENT.C_NOM_ENTIDAD,
			PL.N_ID_RAMO,
			PR.N_ID_PRODUCTO,
			PR.C_DES_PRODUCTO,
			PL.N_ID_PLAN,
			PL.C_COD_PLAN,
			PL.C_DES_CORTAPLAN,
			PL.C_DES_LARGAPLAN,
			PL.C_COD_REFERENCIA,
			PL.C_COD_TRAMA,
			PL.C_VAL_SECPOLIZA,
			PL.C_VAL_POLIZA,
			PL.N_ID_TIPOPOLIZA,
			PL.N_ID_TIPOPAGOPOLIZA,  
			PL.N_ID_TIPOMONEDA,
			PL.N_ID_TIPOREGLATARIFA,
			PL.N_ID_TIPOPLAN,
			PL.N_ID_TIPOVIGENCIA,
			PL.N_ID_TIPOCLIENTE,
			ISNULL(PL.N_ID_TIPOPERSONA,0) AS N_ID_TIPOPERSONA,
			CONVERT(CHAR(10), CER.D_FEC_CERTIFICADO, 103) AS D_FEC_CERTIFICADO,
			CONVERT(CHAR(10), CER.D_FEC_INIVIGENCIA, 103) AS D_FEC_INIVIGENCIA,
			CONVERT(CHAR(10), CER.D_FEC_FINVIGENCIA, 103) AS D_FEC_FINVIGENCIA,
			CONVERT(CHAR(10), CER.D_FEC_CREACION, 103) AS D_FEC_REGISTRO,
			CONVERT(CHAR(10), CER.D_FEC_VENTA, 103) AS D_FEC_VENTA,
			CER.N_NUM_VALORCOMERCIAL,
			CER.N_NUM_PRIMABRUTA,
			CER.N_NUM_PRIMANETA,
			CER.N_NUM_COM_CANAL,
			PL.N_NUM_VERSION,
			CER.C_OBS_DESCRIPTIVO,
			0 AS N_ID_PERSONA,
			0 AS N_ID_ASEGURADO,
			CER.N_IND_ACTIVO,
			CER.N_ID_ESTADO,
			CER.C_COD_USUCREACION,
			CONVERT(CHAR(10), CER.D_FEC_CREACION, 103) AS D_FEC_CREACION,
			CER.C_COD_USUMODIF,
			CONVERT(VARCHAR,CER.D_FEC_MODIFICACION,103) AS D_FEC_MODIFICACION,
			0 AS N_ID_ESTADOTRANSFERENCIA,
			'''' AS C_DES_ESTADOTRANSFERENCIA,
			'''' AS MESHABILITADO,
			CER.N_ID_CANAL,
			CER.N_ID_ESTABLECIMIENTO, 
			'''' AS C_VAL_CODHOMOLOGACION,
			CER.N_ID_PEREMISOR,
			CER.N_ID_PERVENDEDOR,
			ISNULL(CER.N_ID_ADJUNTOPOLIZA,0) AS N_ID_ADJUNTOPOLIZA,
			'''' AS C_MODELO_SUBMODELO,
			CONVERT(CHAR(10), CER.D_FEC_ANULACION, 103) AS D_FEC_ANULACION,
			CER.N_ID_PERANULACION,
			CER.N_ID_MOTIVOANULA,
			CASE WHEN CONVERT(DATE, CER.D_FEC_CERTIFICADO, 103) = CONVERT(DATE, GETDATE(), 103) THEN 1 ELSE 0 END AS N_IND_ANULAR,  
			CASE WHEN CONVERT(DATE, CER.D_FEC_CERTIFICADO, 103) < CONVERT(DATE, GETDATE(), 103)THEN 1 ELSE 0 END AS N_IND_CANCELAR
		FROM T_MAE_CERTIFICADO CER  
		INNER JOIN T_MAE_POLIZA POL ON CER.N_IND_ACTIVO = 1' + @condicional_certificado + @condicional_tercero + ' AND CER.N_ID_POLIZA = POL.N_ID_POLIZA' 
												+ @condicional_poliza + @condicional_estructura_plan + @condicional_estructura_est +
		' INNER JOIN T_MAE_PLAN PL ON POL.N_ID_PLAN = PL.N_ID_PLAN' + @condicional_plan +
		' INNER JOIN T_MAE_PRODUCTO PR ON PL.N_ID_PRODUCTO = PR.N_ID_PRODUCTO' + @condicional_producto + 
		' INNER JOIN T_MAE_ENTIDAD ENT ON PR.N_ID_ENTIDAD = ENT.N_ID_ENTIDAD
		ORDER BY CER.N_ID_CERTIFICADO DESC
		OFFSET ' + CONVERT(VARCHAR,@v_Desde) + ' ROWS FETCH NEXT ' + CONVERT(VARCHAR,@v_CantidadRegistro) + ' ROWS ONLY
		), TABLA_1_5 AS (
		SELECT
			T1.NUMROW
			,T1.ROW_TOTAL
			,T1.N_ID_POLIZA
			,T1.C_NUM_POLIZA
			,T1.N_ID_CERTIFICADO
			,T1.C_NUM_CERTIFICADO
			,T1.N_ID_COTIZACION
            ,T1.N_ID_SUSCRIPCION
			,T1.C_NUM_SOLICITUD
            ,T1.C_NUM_CREDITO
			,T1.N_ID_ENTIDAD
			,T1.C_NOM_ENTIDAD
			,T1.N_ID_RAMO
			,T1.N_ID_PRODUCTO
			,T1.C_DES_PRODUCTO
			,T1.N_ID_PLAN
			,T1.C_COD_PLAN
			,T1.C_DES_CORTAPLAN
			,T1.C_DES_LARGAPLAN
			,T1.C_COD_REFERENCIA
			,T1.C_COD_TRAMA
			,T1.C_VAL_SECPOLIZA
			,T1.C_VAL_POLIZA
			,T1.N_ID_TIPOPOLIZA
			,T1.N_ID_TIPOPAGOPOLIZA
			,T1.N_ID_TIPOMONEDA
			,T1.N_ID_TIPOREGLATARIFA
			,T1.N_ID_TIPOPLAN
			,T1.N_ID_TIPOVIGENCIA
			,T1.N_ID_TIPOCLIENTE
			,T1.N_ID_TIPOPERSONA
			,T1.D_FEC_CERTIFICADO
			,T1.D_FEC_INIVIGENCIA
			,T1.D_FEC_FINVIGENCIA
			,T1.D_FEC_REGISTRO
			,T1.D_FEC_VENTA
			,T1.N_NUM_VALORCOMERCIAL
			,T1.N_NUM_PRIMABRUTA
			,T1.N_NUM_PRIMANETA
			,T1.N_NUM_COM_CANAL
			,T1.N_NUM_VERSION
			,T1.C_OBS_DESCRIPTIVO
			,T1.N_ID_PERSONA
			,T1.N_ID_ASEGURADO
			,T1.N_IND_ACTIVO
			,T1.N_ID_ESTADO
			,T1.C_COD_USUCREACION
			,T1.D_FEC_CREACION
			,T1.C_COD_USUMODIF
			,T1.D_FEC_MODIFICACION
			,T1.N_ID_ESTADOTRANSFERENCIA
			,T1.C_DES_ESTADOTRANSFERENCIA
			,T1.MESHABILITADO
			,T1.N_ID_CANAL
			,ENT.C_NOM_ENTIDAD AS C_NOM_CANAL
			,REG.C_DES_ZONAL AS C_DES_REGION
			,ZON.C_DES_ZONAL AS C_DES_ZONA
			,T1.N_ID_ESTABLECIMIENTO
			,T1.C_VAL_CODHOMOLOGACION
			,EST.C_NOM_ESTABLECIMIENTO AS C_NOM_ESTABLECIMIENTO
			,T1.N_ID_PEREMISOR
			,T1.N_ID_PERVENDEDOR
			,T1.N_ID_ADJUNTOPOLIZA
			,T1.C_MODELO_SUBMODELO
			,T1.D_FEC_ANULACION
			,T1.N_ID_PERANULACION
			,T1.N_ID_MOTIVOANULA
			,T1.N_IND_ANULAR
			,T1.N_IND_CANCELAR
		FROM TABLA_1 T1
		INNER JOIN T_MAE_ENTIDAD ENT ON T1.N_ID_CANAL = ENT.N_ID_ENTIDAD
		LEFT JOIN T_MAE_ESTABLECIMIENTO EST ON T1.N_ID_ESTABLECIMIENTO = EST.N_ID_ESTABLECIMIENTO
		LEFT JOIN T_MAE_ZONAL PVT ON EST.N_ID_ZONAL = PVT.N_ID_ZONAL AND PVT.N_ID_TIPOZONAL = 406 
		LEFT JOIN T_MAE_ZONAL REG ON PVT.N_ID_ZONALPADRE = REG.N_ID_ZONAL AND REG.N_ID_TIPOZONAL = 405 
		LEFT JOIN T_MAE_ZONAL ZON ON REG.N_ID_ZONALPADRE = ZON.N_ID_ZONAL AND ZON.N_ID_TIPOZONAL = 404 
		), TABLA_2 AS (
		SELECT
			T1.NUMROW
			,T1.ROW_TOTAL
			,T1.N_ID_POLIZA
			,T1.C_NUM_POLIZA
			,T1.N_ID_CERTIFICADO
			,T1.C_NUM_CERTIFICADO			
			,COTI.N_ID_COTIZACION
			,COTI.C_NUM_COTIZACION
            ,T1.N_ID_SUSCRIPCION
			,T1.C_NUM_SOLICITUD
            ,T1.C_NUM_CREDITO
			,T1.N_ID_ENTIDAD
			,T1.C_NOM_ENTIDAD
			,T1.N_ID_RAMO
			,T1.N_ID_PRODUCTO
			,T1.C_DES_PRODUCTO
			,T1.N_ID_PLAN
			,T1.C_COD_PLAN
			,T1.C_DES_CORTAPLAN
			,T1.C_DES_LARGAPLAN
			,T1.C_COD_REFERENCIA
			,T1.C_COD_TRAMA
			,T1.C_VAL_SECPOLIZA
			,T1.C_VAL_POLIZA
			,T1.N_ID_TIPOPOLIZA
			,T1.N_ID_TIPOPAGOPOLIZA
			,T1.N_ID_TIPOMONEDA
			,T1.N_ID_TIPOREGLATARIFA
			,T1.N_ID_TIPOPLAN
			,T1.N_ID_TIPOVIGENCIA
			,T1.N_ID_TIPOCLIENTE
			,T1.N_ID_TIPOPERSONA
			,T1.D_FEC_CERTIFICADO
			,T1.D_FEC_INIVIGENCIA
			,T1.D_FEC_FINVIGENCIA
			,T1.D_FEC_REGISTRO
			,T1.D_FEC_VENTA
			,T1.N_NUM_VALORCOMERCIAL
			,T1.N_NUM_PRIMABRUTA
			,T1.N_NUM_PRIMANETA
			,T1.N_NUM_COM_CANAL
			,T1.N_NUM_VERSION
			,T1.C_OBS_DESCRIPTIVO
			,T1.N_ID_PERSONA
			,T1.N_ID_ASEGURADO
			,T1.N_IND_ACTIVO
			,T1.N_ID_ESTADO
			,T1.C_COD_USUCREACION
			,T1.D_FEC_CREACION
			,T1.C_COD_USUMODIF
			,T1.D_FEC_MODIFICACION
			,T1.N_ID_ESTADOTRANSFERENCIA
			,T1.C_DES_ESTADOTRANSFERENCIA
			,T1.MESHABILITADO
			,T1.N_ID_CANAL
			,T1.C_NOM_CANAL
			,T1.C_DES_REGION
			,T1.C_DES_ZONA
			,T1.N_ID_ESTABLECIMIENTO
			,T1.C_VAL_CODHOMOLOGACION
			,T1.C_NOM_ESTABLECIMIENTO
			,T1.N_ID_PEREMISOR
			,T1.N_ID_PERVENDEDOR
			,T1.N_ID_ADJUNTOPOLIZA
			,T1.C_MODELO_SUBMODELO
			,T1.D_FEC_ANULACION
			,T1.N_ID_PERANULACION
			,T1.N_ID_MOTIVOANULA
			,T1.N_IND_ANULAR
			,T1.N_IND_CANCELAR
		FROM TABLA_1_5 T1
		LEFT JOIN T_MAE_COTIZACION COTI ON T1.N_ID_COTIZACION = COTI.N_ID_COTIZACION
	), TABLA_3 AS (
		SELECT
			T2.NUMROW
			,T2.ROW_TOTAL
			,T2.N_ID_POLIZA
			,T2.C_NUM_POLIZA
			,T2.N_ID_CERTIFICADO
			,T2.C_NUM_CERTIFICADO
			,T2.N_ID_COTIZACION
			,T2.C_NUM_COTIZACION
            ,T2.N_ID_SUSCRIPCION
			,T2.C_NUM_SOLICITUD
            ,T2.C_NUM_CREDITO
			,T2.N_ID_ENTIDAD
			,T2.C_NOM_ENTIDAD
			,T2.N_ID_RAMO
			,T2.N_ID_PRODUCTO
			,T2.C_DES_PRODUCTO
			,T2.N_ID_PLAN
			,T2.C_COD_PLAN
			,T2.C_DES_CORTAPLAN
			,T2.C_DES_LARGAPLAN
			,T2.C_COD_REFERENCIA
			,T2.C_COD_TRAMA
			,T2.C_VAL_SECPOLIZA
			,T2.C_VAL_POLIZA
			,T2.N_ID_TIPOPOLIZA
			,T2.N_ID_TIPOPAGOPOLIZA
			,T2.N_ID_TIPOMONEDA
			,T2.N_ID_TIPOREGLATARIFA
			,T2.N_ID_TIPOPLAN
			,T2.N_ID_TIPOVIGENCIA
			,T2.N_ID_TIPOCLIENTE
			,T2.N_ID_TIPOPERSONA
			,T2.D_FEC_CERTIFICADO
			,T2.D_FEC_INIVIGENCIA
			,T2.D_FEC_FINVIGENCIA
			,T2.D_FEC_REGISTRO
			,T2.D_FEC_VENTA
			,T2.N_NUM_VALORCOMERCIAL
			,T2.N_NUM_PRIMABRUTA
			,T2.N_NUM_PRIMANETA
			,T2.N_NUM_COM_CANAL
			,T2.N_NUM_VERSION
			,T2.C_OBS_DESCRIPTIVO
			,0 AS N_ID_PERSONA
			,TER.C_NOM_COMPLETO
			,0 AS N_ID_CONTRATANTE
			,TDO.C_DES_TIPO AS C_TIPO_IDENTIDAD_CONTRATANTE
			,TER.C_VAL_NUMIDENTIDAD AS C_NUM_IDENTIDAD_CONTRATANTE
			,TER.C_NOM_COMPLETO AS C_NOM_COMPLETO_CONTRATANTE
			,TER.N_ID_UBIGEODEPARTAMENTO
			,TER.N_ID_UBIGEOPROVINCIA
			,TER.N_ID_UBIGEODISTRITO
			,TER.N_ID_TIPOVIA
			,TER.C_DES_DIRECCION
			,TER.C_VAL_NUMVIA
			,T2.N_ID_ASEGURADO
			,T2.N_IND_ACTIVO
			,T2.N_ID_ESTADO
			,T2.C_COD_USUCREACION
			,T2.D_FEC_CREACION
			,T2.C_COD_USUMODIF
			,T2.D_FEC_MODIFICACION
			,T2.N_ID_ESTADOTRANSFERENCIA
			,T2.C_DES_ESTADOTRANSFERENCIA
			,T2.MESHABILITADO
			,T2.N_ID_CANAL
			,T2.C_NOM_CANAL
			,T2.C_DES_REGION
			,T2.C_DES_ZONA
			,T2.N_ID_ESTABLECIMIENTO
			,T2.C_VAL_CODHOMOLOGACION
			,T2.C_NOM_ESTABLECIMIENTO
			,T2.N_ID_PEREMISOR
			,T2.N_ID_PERVENDEDOR
			,T2.N_ID_ADJUNTOPOLIZA
			,T2.C_MODELO_SUBMODELO
			,T2.D_FEC_ANULACION
			,T2.N_ID_PERANULACION
			,T2.N_ID_MOTIVOANULA
			,T2.N_IND_ANULAR
			,T2.N_IND_CANCELAR
		FROM TABLA_2 T2
		INNER JOIN T_DET_ROLTERCEROCERTIFICADO RTC ON RTC.N_ID_CERTIFICADO = T2.N_ID_CERTIFICADO AND RTC.N_ID_TIPOROLTERCERO = 364 AND RTC.N_ID_ESTADO = 8 AND RTC.N_IND_ACTIVO = 1
		INNER JOIN T_MAE_TERCERO TER ON RTC.N_ID_TERCERO = TER.N_ID_TERCERO 
		INNER JOIN T_MAE_TIPO TDO ON TER.N_ID_TIPOIDENTIDAD = TDO.N_ID_TIPO
	), TABLA_4 AS (
		SELECT
			T3.NUMROW
			,T3.ROW_TOTAL
			,T3.N_ID_POLIZA
			,T3.C_NUM_POLIZA
			,T3.N_ID_CERTIFICADO
			,T3.C_NUM_CERTIFICADO
			,T3.N_ID_COTIZACION
			,T3.C_NUM_COTIZACION
            ,T3.N_ID_SUSCRIPCION
			,T3.C_NUM_SOLICITUD
            ,T3.C_NUM_CREDITO
			,T3.N_ID_ENTIDAD
			,T3.C_NOM_ENTIDAD
			,T3.N_ID_RAMO
			,T3.N_ID_PRODUCTO
			,T3.C_DES_PRODUCTO
			,T3.N_ID_PLAN
			,T3.C_COD_PLAN
			,T3.C_DES_CORTAPLAN
			,T3.C_DES_LARGAPLAN
			,T3.C_COD_REFERENCIA
			,T3.C_COD_TRAMA
			,T3.C_VAL_SECPOLIZA
			,T3.C_VAL_POLIZA
			,T3.N_ID_TIPOPOLIZA
			,T3.N_ID_TIPOPAGOPOLIZA
			,T3.N_ID_TIPOMONEDA
			,T3.N_ID_TIPOREGLATARIFA
			,T3.N_ID_TIPOPLAN
			,T3.N_ID_TIPOVIGENCIA
			,T3.N_ID_TIPOCLIENTE
			,T3.N_ID_TIPOPERSONA
			,T3.D_FEC_CERTIFICADO
			,T3.D_FEC_INIVIGENCIA
			,T3.D_FEC_FINVIGENCIA
			,T3.D_FEC_REGISTRO
			,T3.D_FEC_VENTA
			,T3.N_NUM_VALORCOMERCIAL
			,T3.N_NUM_PRIMABRUTA
			,T3.N_NUM_PRIMANETA
			,T3.N_NUM_COM_CANAL
			,T3.N_NUM_VERSION
			,T3.C_OBS_DESCRIPTIVO
			,T3.N_ID_PERSONA
			,T3.C_NOM_COMPLETO
			,T3.N_ID_CONTRATANTE
			,T3.C_TIPO_IDENTIDAD_CONTRATANTE
			,T3.C_NUM_IDENTIDAD_CONTRATANTE
			,T3.C_NOM_COMPLETO_CONTRATANTE
			,T3.N_ID_UBIGEODEPARTAMENTO
			,T3.N_ID_UBIGEOPROVINCIA
			,T3.N_ID_UBIGEODISTRITO
			,T3.N_ID_TIPOVIA
			,T3.C_DES_DIRECCION
			,T3.C_VAL_NUMVIA
			,T3.N_ID_ASEGURADO
			,TER.N_ID_TIPOIDENTIDAD AS N_ID_TIPO_IDENTIDAD_ASEGURADO
			,TDO.C_DES_TIPO AS C_TIPO_IDENTIDAD_ASEGURADO
			,TER.C_VAL_NUMIDENTIDAD AS C_NUM_IDENTIDAD_ASEGURADO
			,ISNULL(TER.C_APE_PATERNO,'''') AS C_APE_PATERNO_ASEGURADO
			,ISNULL(TER.C_APE_MATERNO,'''') AS C_APE_MATERNO_ASEGURADO
			,ISNULL(TER.C_NOM_TERCERO,'''') AS C_NOMBRE_ASEGURADO
			,ISNULL(TER.C_NOM_COMPLETO,'''') AS C_NOM_COMPLETO_ASEGURADO
			,ISNULL(TER.C_VAL_CELULAR,'''') AS C_CELULAR_ASEGURADO
			,ISNULL(TER.C_DIR_CORREO,'''') C_CORREO_ASEGURADO
			,T3.N_IND_ACTIVO
			,T3.N_ID_ESTADO
			,T3.C_COD_USUCREACION
			,T3.D_FEC_CREACION
			,T3.C_COD_USUMODIF
			,T3.D_FEC_MODIFICACION
			,T3.N_ID_ESTADOTRANSFERENCIA
			,T3.C_DES_ESTADOTRANSFERENCIA
			,T3.MESHABILITADO
			,T3.N_ID_CANAL
			,T3.C_NOM_CANAL
			,T3.C_DES_REGION
			,T3.C_DES_ZONA
			,T3.N_ID_ESTABLECIMIENTO
			,T3.C_VAL_CODHOMOLOGACION
			,T3.C_NOM_ESTABLECIMIENTO
			,T3.N_ID_PEREMISOR
			,T3.N_ID_PERVENDEDOR
			,T3.N_ID_ADJUNTOPOLIZA
			,T3.C_MODELO_SUBMODELO
			,T3.D_FEC_ANULACION
			,T3.N_ID_PERANULACION
			,T3.N_ID_MOTIVOANULA
			,T3.N_IND_ANULAR
			,T3.N_IND_CANCELAR
		FROM TABLA_3 T3
		INNER JOIN T_DET_ROLTERCEROCERTIFICADO RTA ON RTA.N_ID_CERTIFICADO = T3.N_ID_CERTIFICADO AND RTA.N_ID_TIPOROLTERCERO = 365 AND RTA.N_ID_ESTADO = 8 AND RTA.N_IND_ACTIVO = 1
		INNER JOIN T_MAE_TERCERO TER ON RTA.N_ID_TERCERO = TER.N_ID_TERCERO 
		INNER JOIN T_MAE_TIPO TDO ON TER.N_ID_TIPOIDENTIDAD = TDO.N_ID_TIPO
	), TABLA_5 AS (
		SELECT
			T4.NUMROW
			,T4.ROW_TOTAL
			,T4.N_ID_POLIZA
			,T4.C_NUM_POLIZA
			,T4.N_ID_CERTIFICADO
			,T4.C_NUM_CERTIFICADO
			,T4.N_ID_COTIZACION
			,T4.C_NUM_COTIZACION
            ,T4.N_ID_SUSCRIPCION
			,T4.C_NUM_SOLICITUD
            ,T4.C_NUM_CREDITO
			,T4.N_ID_ENTIDAD
			,T4.C_NOM_ENTIDAD
			,T4.N_ID_RAMO
			,T4.N_ID_PRODUCTO
			,T4.C_DES_PRODUCTO
			,T4.N_ID_PLAN
			,T4.C_COD_PLAN
			,T4.C_DES_CORTAPLAN
			,T4.C_DES_LARGAPLAN
			,T4.C_COD_REFERENCIA
			,T4.C_COD_TRAMA
			,T4.C_VAL_SECPOLIZA
			,T4.C_VAL_POLIZA
			,T4.N_ID_TIPOPOLIZA
			,T4.N_ID_TIPOPAGOPOLIZA
			,T4.N_ID_TIPOMONEDA
			,T4.N_ID_TIPOREGLATARIFA
			,T4.N_ID_TIPOPLAN
			,T4.N_ID_TIPOVIGENCIA
			,T4.N_ID_TIPOCLIENTE
			,T4.N_ID_TIPOPERSONA
			,T4.D_FEC_CERTIFICADO
			,T4.D_FEC_INIVIGENCIA
			,T4.D_FEC_FINVIGENCIA
			,T4.D_FEC_REGISTRO
			,T4.D_FEC_VENTA
			,T4.N_NUM_VALORCOMERCIAL
			,T4.N_NUM_PRIMABRUTA
			,T4.N_NUM_PRIMANETA
			,T4.N_NUM_COM_CANAL
			,T4.N_NUM_VERSION
			,T4.C_OBS_DESCRIPTIVO
			,T4.N_ID_PERSONA
			,T4.C_NOM_COMPLETO
			,T4.N_ID_CONTRATANTE
			,T4.C_TIPO_IDENTIDAD_CONTRATANTE
			,T4.C_NUM_IDENTIDAD_CONTRATANTE
			,T4.C_NOM_COMPLETO_CONTRATANTE
			,T4.N_ID_UBIGEODEPARTAMENTO
			,T4.N_ID_UBIGEOPROVINCIA
			,T4.N_ID_UBIGEODISTRITO
			,T4.N_ID_TIPOVIA
			,T4.C_DES_DIRECCION
			,T4.C_VAL_NUMVIA
			,T4.N_ID_ASEGURADO
			,T4.N_ID_TIPO_IDENTIDAD_ASEGURADO
			,T4.C_TIPO_IDENTIDAD_ASEGURADO
			,T4.C_NUM_IDENTIDAD_ASEGURADO
			,T4.C_APE_PATERNO_ASEGURADO
			,T4.C_APE_MATERNO_ASEGURADO
			,T4.C_NOMBRE_ASEGURADO
			,T4.C_NOM_COMPLETO_ASEGURADO
			,T4.C_CELULAR_ASEGURADO
			,T4.C_CORREO_ASEGURADO
			,0  AS N_ID_RESPONSABLEPAGO
			,TDO.C_DES_TIPO AS C_TIPO_IDENTIDAD_RESPONSABLEPAGO
			,TER.C_VAL_NUMIDENTIDAD AS C_NUM_IDENTIDAD_RESPONSABLEPAGO
			,TER.C_NOM_COMPLETO AS C_NOM_COMPLETO_RESPONSABLEPAGO
			,T4.N_IND_ACTIVO
			,T4.N_ID_ESTADO
			,T4.C_COD_USUCREACION
			,T4.D_FEC_CREACION
			,T4.C_COD_USUMODIF
			,T4.D_FEC_MODIFICACION
			,T4.N_ID_ESTADOTRANSFERENCIA
			,T4.C_DES_ESTADOTRANSFERENCIA
			,T4.MESHABILITADO
			,T4.N_ID_CANAL
			,T4.C_NOM_CANAL
			,T4.C_DES_REGION
			,T4.C_DES_ZONA
			,T4.N_ID_ESTABLECIMIENTO
			,T4.C_VAL_CODHOMOLOGACION
			,T4.C_NOM_ESTABLECIMIENTO
			,T4.N_ID_PEREMISOR
			,T4.N_ID_PERVENDEDOR
			,T4.N_ID_ADJUNTOPOLIZA
			,T4.C_MODELO_SUBMODELO
			,T4.D_FEC_ANULACION
			,T4.N_ID_PERANULACION
			,T4.N_ID_MOTIVOANULA
			,T4.N_IND_ANULAR
			,T4.N_IND_CANCELAR
		FROM TABLA_4 T4
		INNER JOIN T_DET_ROLTERCEROCERTIFICADO RTR ON RTR.N_ID_CERTIFICADO = T4.N_ID_CERTIFICADO AND RTR.N_ID_TIPOROLTERCERO = 366 AND RTR.N_ID_ESTADO = 8 AND RTR.N_IND_ACTIVO = 1
		INNER JOIN T_MAE_TERCERO TER ON RTR.N_ID_TERCERO = TER.N_ID_TERCERO 
		INNER JOIN T_MAE_TIPO TDO ON TER.N_ID_TIPOIDENTIDAD = TDO.N_ID_TIPO
	), TABLA_6 AS (
		SELECT
			T5.NUMROW
			,T5.ROW_TOTAL
			,T5.N_ID_POLIZA
			,T5.C_NUM_POLIZA
			,T5.N_ID_CERTIFICADO
			,T5.C_NUM_CERTIFICADO
			,T5.N_ID_COTIZACION
			,T5.C_NUM_COTIZACION
            ,T5.N_ID_SUSCRIPCION
            ,T5.C_NUM_SOLICITUD
			,T5.C_NUM_CREDITO
			,T5.N_ID_ENTIDAD
			,T5.C_NOM_ENTIDAD
			,T5.N_ID_RAMO
			,T5.N_ID_PRODUCTO
			,T5.C_DES_PRODUCTO
			,T5.N_ID_PLAN
			,T5.C_COD_PLAN
			,T5.C_DES_CORTAPLAN
			,T5.C_DES_LARGAPLAN
			,T5.C_COD_REFERENCIA
			,T5.C_COD_TRAMA
			,T5.C_VAL_SECPOLIZA
			,T5.C_VAL_POLIZA
			,T5.N_ID_TIPOPOLIZA
			,T5.N_ID_TIPOPAGOPOLIZA
			,T5.N_ID_TIPOMONEDA
			,T5.N_ID_TIPOREGLATARIFA
			,T5.N_ID_TIPOPLAN
			,T5.N_ID_TIPOVIGENCIA
			,T5.N_ID_TIPOCLIENTE
			,T5.N_ID_TIPOPERSONA
			,T5.D_FEC_CERTIFICADO
			,T5.D_FEC_INIVIGENCIA
			,T5.D_FEC_FINVIGENCIA
			,T5.D_FEC_REGISTRO
			,T5.D_FEC_VENTA
			,T5.N_NUM_VALORCOMERCIAL
			,T5.N_NUM_PRIMABRUTA
			,T5.N_NUM_PRIMANETA
			,T5.N_NUM_COM_CANAL
			,T5.N_NUM_VERSION
			,T5.C_OBS_DESCRIPTIVO
			,T5.N_ID_PERSONA
			,T5.C_NOM_COMPLETO
			,T5.N_ID_CONTRATANTE
			,T5.C_TIPO_IDENTIDAD_CONTRATANTE
			,T5.C_NUM_IDENTIDAD_CONTRATANTE
			,T5.C_NOM_COMPLETO_CONTRATANTE
			,T5.N_ID_UBIGEODEPARTAMENTO
			,T5.N_ID_UBIGEOPROVINCIA
			,T5.N_ID_UBIGEODISTRITO
			,T5.N_ID_TIPOVIA
			,T5.C_DES_DIRECCION
			,T5.C_VAL_NUMVIA
			,T5.N_ID_ASEGURADO
			,T5.N_ID_TIPO_IDENTIDAD_ASEGURADO
			,T5.C_TIPO_IDENTIDAD_ASEGURADO
			,T5.C_NUM_IDENTIDAD_ASEGURADO
			,T5.C_APE_PATERNO_ASEGURADO
			,T5.C_APE_MATERNO_ASEGURADO
			,T5.C_NOMBRE_ASEGURADO
			,T5.C_NOM_COMPLETO_ASEGURADO
			,T5.C_CELULAR_ASEGURADO
			,T5.C_CORREO_ASEGURADO
			,T5.N_ID_RESPONSABLEPAGO
			,T5.C_TIPO_IDENTIDAD_RESPONSABLEPAGO
			,T5.C_NUM_IDENTIDAD_RESPONSABLEPAGO
			,T5.C_NOM_COMPLETO_RESPONSABLEPAGO
			,TMO.C_DES_TIPO AS C_DES_TIPOMONEDA
			,TMO.C_ABR_TIPO AS C_ABR_TIPOMONEDA
			,T5.N_IND_ACTIVO
			,T5.N_ID_ESTADO
			,T5.C_COD_USUCREACION
			,T5.D_FEC_CREACION
			,T5.C_COD_USUMODIF
			,T5.D_FEC_MODIFICACION
			,T5.N_ID_ESTADOTRANSFERENCIA
			,T5.C_DES_ESTADOTRANSFERENCIA
			,T5.MESHABILITADO
			,T5.N_ID_CANAL
			,T5.C_NOM_CANAL
			,T5.C_DES_REGION
			,T5.C_DES_ZONA
			,T5.N_ID_ESTABLECIMIENTO
			,T5.C_VAL_CODHOMOLOGACION
			,T5.C_NOM_ESTABLECIMIENTO
			,T5.N_ID_PEREMISOR
			,T5.N_ID_PERVENDEDOR
			,T5.N_ID_ADJUNTOPOLIZA
			,T5.C_MODELO_SUBMODELO
			,T5.D_FEC_ANULACION
			,T5.N_ID_PERANULACION
			,T5.N_ID_MOTIVOANULA
			,T5.N_IND_ANULAR
			,T5.N_IND_CANCELAR
		FROM TABLA_5 T5
		INNER JOIN T_MAE_TIPO TMO ON T5.N_ID_TIPOMONEDA = TMO.N_ID_TIPO
	), TABLA_7 AS (
		SELECT
			T6.NUMROW
			,T6.ROW_TOTAL
			,T6.N_ID_POLIZA
			,T6.C_NUM_POLIZA
			,T6.N_ID_CERTIFICADO
			,T6.C_NUM_CERTIFICADO
			,T6.N_ID_COTIZACION
			,T6.C_NUM_COTIZACION
            ,T6.N_ID_SUSCRIPCION
			,T6.C_NUM_SOLICITUD
            ,T6.C_NUM_CREDITO
			,T6.N_ID_ENTIDAD
			,T6.C_NOM_ENTIDAD
			,T6.N_ID_RAMO
			,T6.N_ID_PRODUCTO
			,T6.C_DES_PRODUCTO
			,T6.N_ID_PLAN
			,T6.C_COD_PLAN
			,T6.C_DES_CORTAPLAN
			,T6.C_DES_LARGAPLAN
			,T6.C_COD_REFERENCIA
			,T6.C_COD_TRAMA
			,T6.C_VAL_SECPOLIZA
			,T6.C_VAL_POLIZA
			,T6.N_ID_TIPOPOLIZA
			,T6.N_ID_TIPOPAGOPOLIZA
			,T6.N_ID_TIPOMONEDA
			,TVIG.C_DES_TIPO AS C_DES_TIPOVIGENCIA
			,T6.N_ID_TIPOREGLATARIFA
			,T6.N_ID_TIPOPLAN
			,T6.N_ID_TIPOVIGENCIA
			,T6.N_ID_TIPOCLIENTE
			,T6.N_ID_TIPOPERSONA
			,T6.D_FEC_CERTIFICADO
			,T6.D_FEC_INIVIGENCIA
			,T6.D_FEC_FINVIGENCIA
			,T6.D_FEC_REGISTRO
			,T6.D_FEC_VENTA
			,T6.N_NUM_VALORCOMERCIAL
			,T6.N_NUM_PRIMABRUTA
			,T6.N_NUM_PRIMANETA
			,T6.N_NUM_COM_CANAL
			,T6.N_NUM_VERSION
			,T6.C_OBS_DESCRIPTIVO
			,T6.N_ID_PERSONA
			,T6.C_NOM_COMPLETO
			,T6.N_ID_CONTRATANTE
			,T6.C_TIPO_IDENTIDAD_CONTRATANTE
			,T6.C_NUM_IDENTIDAD_CONTRATANTE
			,T6.C_NOM_COMPLETO_CONTRATANTE
			,T6.N_ID_UBIGEODEPARTAMENTO
			,T6.N_ID_UBIGEOPROVINCIA
			,T6.N_ID_UBIGEODISTRITO
			,T6.N_ID_TIPOVIA
			,T6.C_DES_DIRECCION
			,T6.C_VAL_NUMVIA
			,T6.N_ID_ASEGURADO
			,T6.N_ID_TIPO_IDENTIDAD_ASEGURADO
			,T6.C_TIPO_IDENTIDAD_ASEGURADO
			,T6.C_NUM_IDENTIDAD_ASEGURADO
			,T6.C_APE_PATERNO_ASEGURADO
			,T6.C_APE_MATERNO_ASEGURADO
			,T6.C_NOMBRE_ASEGURADO
			,T6.C_NOM_COMPLETO_ASEGURADO
			,T6.C_CELULAR_ASEGURADO
			,T6.C_CORREO_ASEGURADO
			,T6.N_ID_RESPONSABLEPAGO
			,T6.C_TIPO_IDENTIDAD_RESPONSABLEPAGO
			,T6.C_NUM_IDENTIDAD_RESPONSABLEPAGO
			,T6.C_NOM_COMPLETO_RESPONSABLEPAGO
			,T6.C_DES_TIPOMONEDA
			,T6.C_ABR_TIPOMONEDA
			,T6.N_IND_ACTIVO
			,T6.N_ID_ESTADO
			,T6.C_COD_USUCREACION
			,T6.D_FEC_CREACION
			,T6.C_COD_USUMODIF
			,T6.D_FEC_MODIFICACION
			,T6.N_ID_ESTADOTRANSFERENCIA
			,T6.C_DES_ESTADOTRANSFERENCIA
			,T6.MESHABILITADO
			,T6.N_ID_CANAL
			,T6.C_NOM_CANAL
			,T6.C_DES_REGION
			,T6.C_DES_ZONA
			,T6.N_ID_ESTABLECIMIENTO
			,T6.C_VAL_CODHOMOLOGACION
			,T6.C_NOM_ESTABLECIMIENTO
			,T6.N_ID_PEREMISOR
			,T6.N_ID_PERVENDEDOR
			,T6.N_ID_ADJUNTOPOLIZA
			,T6.C_MODELO_SUBMODELO
			,T6.D_FEC_ANULACION
			,T6.N_ID_PERANULACION
			,T6.N_ID_MOTIVOANULA
			,T6.N_IND_ANULAR
			,T6.N_IND_CANCELAR
		FROM TABLA_6 T6
		INNER JOIN T_MAE_TIPO TVIG ON TVIG.N_ID_TIPO=T6.N_ID_TIPOVIGENCIA
	), TABLA_8 AS (
		SELECT
			T7.NUMROW
			,T7.ROW_TOTAL
			,T7.N_ID_POLIZA
			,T7.C_NUM_POLIZA
			,T7.N_ID_CERTIFICADO
			,T7.C_NUM_CERTIFICADO
			,T7.N_ID_COTIZACION
			,T7.C_NUM_COTIZACION
            ,T7.N_ID_SUSCRIPCION
			,T7.C_NUM_SOLICITUD
            ,T7.C_NUM_CREDITO
			,T7.N_ID_ENTIDAD
			,T7.C_NOM_ENTIDAD
			,T7.N_ID_RAMO
			,T7.N_ID_PRODUCTO
			,T7.C_DES_PRODUCTO
			,T7.N_ID_PLAN
			,T7.C_COD_PLAN
			,T7.C_DES_CORTAPLAN
			,T7.C_DES_LARGAPLAN
			,T7.C_COD_REFERENCIA
			,T7.C_COD_TRAMA
			,T7.C_VAL_SECPOLIZA
			,T7.C_VAL_POLIZA
			,T7.N_ID_TIPOPOLIZA
			,T7.N_ID_TIPOPAGOPOLIZA
			,T7.N_ID_TIPOMONEDA
			,T7.C_DES_TIPOVIGENCIA
			,T7.N_ID_TIPOREGLATARIFA
			,T7.N_ID_TIPOPLAN
			,T7.N_ID_TIPOVIGENCIA
			,T7.N_ID_TIPOCLIENTE
			,T7.N_ID_TIPOPERSONA
			,T7.D_FEC_CERTIFICADO
			,T7.D_FEC_INIVIGENCIA
			,T7.D_FEC_FINVIGENCIA
			,T7.D_FEC_REGISTRO
			,T7.D_FEC_VENTA
			,T7.N_NUM_VALORCOMERCIAL
			,T7.N_NUM_PRIMABRUTA
			,T7.N_NUM_PRIMANETA
			,T7.N_NUM_COM_CANAL
			,T7.N_NUM_VERSION
			,T7.C_OBS_DESCRIPTIVO
			,T7.N_ID_PERSONA
			,T7.C_NOM_COMPLETO
			,T7.N_ID_CONTRATANTE
			,T7.C_TIPO_IDENTIDAD_CONTRATANTE
			,T7.C_NUM_IDENTIDAD_CONTRATANTE
			,T7.C_NOM_COMPLETO_CONTRATANTE
			,T7.N_ID_UBIGEODEPARTAMENTO
			,T7.N_ID_UBIGEOPROVINCIA
			,T7.N_ID_UBIGEODISTRITO
			,T7.N_ID_TIPOVIA
			,T7.C_DES_DIRECCION
			,T7.C_VAL_NUMVIA
			,T7.N_ID_ASEGURADO
			,T7.N_ID_TIPO_IDENTIDAD_ASEGURADO
			,T7.C_TIPO_IDENTIDAD_ASEGURADO
			,T7.C_NUM_IDENTIDAD_ASEGURADO
			,T7.C_APE_PATERNO_ASEGURADO
			,T7.C_APE_MATERNO_ASEGURADO
			,T7.C_NOMBRE_ASEGURADO
			,T7.C_NOM_COMPLETO_ASEGURADO
			,T7.C_CELULAR_ASEGURADO
			,T7.C_CORREO_ASEGURADO
			,T7.N_ID_RESPONSABLEPAGO
			,T7.C_TIPO_IDENTIDAD_RESPONSABLEPAGO
			,T7.C_NUM_IDENTIDAD_RESPONSABLEPAGO
			,T7.C_NOM_COMPLETO_RESPONSABLEPAGO
			,T7.C_DES_TIPOMONEDA
			,T7.C_ABR_TIPOMONEDA
			,T7.N_IND_ACTIVO
			,T7.N_ID_ESTADO
			,T7.C_COD_USUCREACION
			,T7.D_FEC_CREACION
			,T7.C_COD_USUMODIF
			,T7.D_FEC_MODIFICACION
			,T7.N_ID_ESTADOTRANSFERENCIA
			,T7.C_DES_ESTADOTRANSFERENCIA
			,T7.MESHABILITADO
			,T7.N_ID_CANAL
			,T7.C_NOM_CANAL
			,T7.C_DES_REGION
			,T7.C_DES_ZONA
			,T7.N_ID_ESTABLECIMIENTO
			,T7.C_VAL_CODHOMOLOGACION
			,T7.C_NOM_ESTABLECIMIENTO
			,T7.N_ID_PEREMISOR
			,PERSE.C_NOM_COMPLETO AS C_NOM_COMPLETO_EMISOR
			,T7.N_ID_PERVENDEDOR
			,T7.N_ID_ADJUNTOPOLIZA
			,T7.C_MODELO_SUBMODELO
			,T7.D_FEC_ANULACION
			,T7.N_ID_PERANULACION
			,T7.N_ID_MOTIVOANULA
			,T7.N_IND_ANULAR
			,T7.N_IND_CANCELAR
		FROM TABLA_7 T7
		LEFT JOIN T_MAE_PERSONA PERSE ON PERSE.N_ID_PERSONA = T7.N_ID_PEREMISOR
	), TABLA_9 AS (
		SELECT
			T8.NUMROW
			,T8.ROW_TOTAL
			,T8.N_ID_POLIZA
			,T8.C_NUM_POLIZA
			,T8.N_ID_CERTIFICADO
			,T8.C_NUM_CERTIFICADO
			,T8.N_ID_COTIZACION
			,T8.C_NUM_COTIZACION
			,T8.N_ID_SUSCRIPCION
			,T8.C_NUM_SOLICITUD
            ,T8.C_NUM_CREDITO
            ,T8.N_ID_ENTIDAD
			,T8.C_NOM_ENTIDAD
			,T8.N_ID_RAMO
			,T8.N_ID_PRODUCTO
			,T8.C_DES_PRODUCTO
			,T8.N_ID_PLAN
			,T8.C_COD_PLAN
			,T8.C_DES_CORTAPLAN
			,T8.C_DES_LARGAPLAN
			,T8.C_COD_REFERENCIA
			,T8.C_COD_TRAMA
			,T8.C_VAL_SECPOLIZA
			,T8.C_VAL_POLIZA
			,T8.N_ID_TIPOPOLIZA
			,T8.N_ID_TIPOPAGOPOLIZA
			,T8.N_ID_TIPOMONEDA
			,T8.C_DES_TIPOVIGENCIA
			,T8.N_ID_TIPOREGLATARIFA
			,T8.N_ID_TIPOPLAN
			,T8.N_ID_TIPOVIGENCIA
			,T8.N_ID_TIPOCLIENTE
			,T8.N_ID_TIPOPERSONA
			,T8.D_FEC_CERTIFICADO
			,T8.D_FEC_INIVIGENCIA
			,T8.D_FEC_FINVIGENCIA
			,T8.D_FEC_REGISTRO
			,T8.D_FEC_VENTA
			,T8.N_NUM_VALORCOMERCIAL
			,T8.N_NUM_PRIMABRUTA
			,T8.N_NUM_PRIMANETA
			,T8.N_NUM_COM_CANAL
			,T8.N_NUM_VERSION
			,T8.C_OBS_DESCRIPTIVO
			,T8.N_ID_PERSONA
			,T8.C_NOM_COMPLETO
			,T8.N_ID_CONTRATANTE
			,T8.C_TIPO_IDENTIDAD_CONTRATANTE
			,T8.C_NUM_IDENTIDAD_CONTRATANTE
			,T8.C_NOM_COMPLETO_CONTRATANTE
			,T8.N_ID_UBIGEODEPARTAMENTO
			,T8.N_ID_UBIGEOPROVINCIA
			,T8.N_ID_UBIGEODISTRITO
			,T8.N_ID_TIPOVIA
			,T8.C_DES_DIRECCION
			,T8.C_VAL_NUMVIA
			,T8.N_ID_ASEGURADO
			,T8.N_ID_TIPO_IDENTIDAD_ASEGURADO
			,T8.C_TIPO_IDENTIDAD_ASEGURADO
			,T8.C_NUM_IDENTIDAD_ASEGURADO
			,T8.C_APE_PATERNO_ASEGURADO
			,T8.C_APE_MATERNO_ASEGURADO
			,T8.C_NOMBRE_ASEGURADO
			,T8.C_NOM_COMPLETO_ASEGURADO
			,T8.C_CELULAR_ASEGURADO
			,T8.C_CORREO_ASEGURADO
			,T8.N_ID_RESPONSABLEPAGO
			,T8.C_TIPO_IDENTIDAD_RESPONSABLEPAGO
			,T8.C_NUM_IDENTIDAD_RESPONSABLEPAGO
			,T8.C_NOM_COMPLETO_RESPONSABLEPAGO
			,T8.C_DES_TIPOMONEDA
			,T8.C_ABR_TIPOMONEDA
			,T8.N_IND_ACTIVO
			,T8.N_ID_ESTADO
			,T8.C_COD_USUCREACION
			,T8.D_FEC_CREACION
			,T8.C_COD_USUMODIF
			,T8.D_FEC_MODIFICACION
			,T8.N_ID_ESTADOTRANSFERENCIA
			,T8.C_DES_ESTADOTRANSFERENCIA
			,T8.MESHABILITADO
			,T8.N_ID_CANAL
			,T8.C_NOM_CANAL
			,T8.C_DES_REGION
			,T8.C_DES_ZONA
			,T8.N_ID_ESTABLECIMIENTO
			,T8.C_VAL_CODHOMOLOGACION
			,T8.C_NOM_ESTABLECIMIENTO
			,T8.N_ID_PEREMISOR
			,T8.C_NOM_COMPLETO_EMISOR
			,T8.N_ID_PERVENDEDOR
			,PERSV.C_VAL_CODREFERENCIA
			,PERSV.C_NOM_COMPLETO AS C_NOM_COMPLETO_VENDEDOR
			,UPPER(TIPOVE.C_DES_TIPO) AS C_TIPO_FUNCIONARIO 
			,T8.N_ID_ADJUNTOPOLIZA
			,T8.C_MODELO_SUBMODELO
			,T8.D_FEC_ANULACION
			,T8.N_ID_PERANULACION
			,T8.N_ID_MOTIVOANULA
			,T8.N_IND_ANULAR
			,T8.N_IND_CANCELAR
		FROM TABLA_8 T8
		LEFT JOIN T_MAE_PERSONA PERSV ON PERSV.N_ID_PERSONA = T8.N_ID_PERVENDEDOR  
		LEFT JOIN T_DET_PERSONA DTPERV ON PERSV.N_ID_PERSONA = DTPERV.N_ID_PERSONA 
		LEFT JOIN T_MAE_TIPO TIPOVE ON TIPOVE.N_ID_TIPO=DTPERV.N_ID_TIPOROL  
	), TABLA_10 AS (
		SELECT
			T9.NUMROW
			,T9.ROW_TOTAL
			,T9.N_ID_POLIZA
			,T9.C_NUM_POLIZA
			,T9.N_ID_CERTIFICADO
			,T9.C_NUM_CERTIFICADO
			,T9.N_ID_COTIZACION
			,T9.C_NUM_COTIZACION
            ,T9.N_ID_SUSCRIPCION
			,T9.C_NUM_SOLICITUD
            ,T9.C_NUM_CREDITO
			,T9.N_ID_ENTIDAD
			,T9.C_NOM_ENTIDAD
			,T9.N_ID_RAMO
			,T9.N_ID_PRODUCTO
			,T9.C_DES_PRODUCTO
			,T9.N_ID_PLAN
			,T9.C_COD_PLAN
			,T9.C_DES_CORTAPLAN
			,T9.C_DES_LARGAPLAN
			,T9.C_COD_REFERENCIA
			,T9.C_COD_TRAMA
			,T9.C_VAL_SECPOLIZA
			,T9.C_VAL_POLIZA
			,T9.N_ID_TIPOPOLIZA
			,T9.N_ID_TIPOPAGOPOLIZA
			,T9.N_ID_TIPOMONEDA
			,T9.C_DES_TIPOVIGENCIA
			,T9.N_ID_TIPOREGLATARIFA
			,T9.N_ID_TIPOPLAN
			,T9.N_ID_TIPOVIGENCIA
			,T9.N_ID_TIPOCLIENTE
			,T9.N_ID_TIPOPERSONA
			,T9.D_FEC_CERTIFICADO
			,T9.D_FEC_INIVIGENCIA
			,T9.D_FEC_FINVIGENCIA
			,T9.D_FEC_REGISTRO
			,T9.D_FEC_VENTA
			,T9.N_NUM_VALORCOMERCIAL
			,T9.N_NUM_PRIMABRUTA
			,T9.N_NUM_PRIMANETA
			,T9.N_NUM_COM_CANAL
			,T9.N_NUM_VERSION
			,T9.C_OBS_DESCRIPTIVO
			,T9.N_ID_PERSONA
			,T9.C_NOM_COMPLETO
			,T9.N_ID_CONTRATANTE
			,T9.C_TIPO_IDENTIDAD_CONTRATANTE
			,T9.C_NUM_IDENTIDAD_CONTRATANTE
			,T9.C_NOM_COMPLETO_CONTRATANTE
			,T9.N_ID_UBIGEODEPARTAMENTO
			,T9.N_ID_UBIGEOPROVINCIA
			,T9.N_ID_UBIGEODISTRITO
			,T9.N_ID_TIPOVIA
			,T9.C_DES_DIRECCION
			,T9.C_VAL_NUMVIA
			,T9.N_ID_ASEGURADO
			,T9.N_ID_TIPO_IDENTIDAD_ASEGURADO
			,T9.C_TIPO_IDENTIDAD_ASEGURADO
			,T9.C_NUM_IDENTIDAD_ASEGURADO
			,T9.C_APE_PATERNO_ASEGURADO
			,T9.C_APE_MATERNO_ASEGURADO
			,T9.C_NOMBRE_ASEGURADO
			,T9.C_NOM_COMPLETO_ASEGURADO
			,T9.C_CELULAR_ASEGURADO
			,T9.C_CORREO_ASEGURADO
			,T9.N_ID_RESPONSABLEPAGO
			,T9.C_TIPO_IDENTIDAD_RESPONSABLEPAGO
			,T9.C_NUM_IDENTIDAD_RESPONSABLEPAGO
			,T9.C_NOM_COMPLETO_RESPONSABLEPAGO
			,T9.C_DES_TIPOMONEDA
			,T9.C_ABR_TIPOMONEDA
			,T9.N_IND_ACTIVO
			,T9.N_ID_ESTADO
			,T9.C_COD_USUCREACION
			,T9.D_FEC_CREACION
			,T9.C_COD_USUMODIF
			,T9.D_FEC_MODIFICACION
			,T9.N_ID_ESTADOTRANSFERENCIA
			,T9.C_DES_ESTADOTRANSFERENCIA
			,T9.MESHABILITADO
			,T9.N_ID_CANAL
			,T9.C_NOM_CANAL
			,T9.C_DES_REGION
			,T9.C_DES_ZONA
			,T9.N_ID_ESTABLECIMIENTO
			,T9.C_VAL_CODHOMOLOGACION
			,T9.C_NOM_ESTABLECIMIENTO
			,T9.N_ID_PEREMISOR
			,T9.C_NOM_COMPLETO_EMISOR
			,T9.N_ID_PERVENDEDOR
			,T9.C_VAL_CODREFERENCIA
			,T9.C_NOM_COMPLETO_VENDEDOR
			,T9.C_TIPO_FUNCIONARIO
			,T9.N_ID_ADJUNTOPOLIZA
			,ISNULL(ADJP.C_DES_ADJUNTO,'''') AS C_DES_ADJUNTOPOLIZA
			,T9.C_MODELO_SUBMODELO
			,T9.D_FEC_ANULACION
			,T9.N_ID_PERANULACION
			,T9.N_ID_MOTIVOANULA
			,T9.N_IND_ANULAR
			,T9.N_IND_CANCELAR
		FROM TABLA_9 T9
		LEFT JOIN T_MAE_ADJUNTO ADJP ON ADJP.N_ID_ADJUNTO = T9.N_ID_ADJUNTOPOLIZA   
	),TABLA_11 AS (
		SELECT
			T10.NUMROW
			,T10.ROW_TOTAL
			,T10.N_ID_POLIZA
			,T10.C_NUM_POLIZA
			,T10.N_ID_CERTIFICADO
			,T10.C_NUM_CERTIFICADO
			,T10.N_ID_COTIZACION
			,T10.C_NUM_COTIZACION
            ,T10.N_ID_SUSCRIPCION
			,T10.C_NUM_SOLICITUD
            ,T10.C_NUM_CREDITO
			,T10.N_ID_ENTIDAD
			,T10.C_NOM_ENTIDAD
			,T10.N_ID_RAMO
			,T10.N_ID_PRODUCTO
			,T10.C_DES_PRODUCTO
			,T10.N_ID_PLAN
			,T10.C_COD_PLAN
			,T10.C_DES_CORTAPLAN
			,T10.C_DES_LARGAPLAN
			,T10.C_COD_REFERENCIA
			,T10.C_COD_TRAMA
			,T10.C_VAL_SECPOLIZA
			,T10.C_VAL_POLIZA
			,T10.N_ID_TIPOPOLIZA
			,T10.N_ID_TIPOPAGOPOLIZA
			,T10.N_ID_TIPOMONEDA
			,T10.C_DES_TIPOVIGENCIA
			,T10.N_ID_TIPOREGLATARIFA
			,T10.N_ID_TIPOPLAN
			,T10.N_ID_TIPOVIGENCIA
			,T10.N_ID_TIPOCLIENTE
			,T10.N_ID_TIPOPERSONA
			,T10.D_FEC_CERTIFICADO
			,T10.D_FEC_INIVIGENCIA
			,T10.D_FEC_FINVIGENCIA
			,T10.D_FEC_REGISTRO
			,T10.D_FEC_VENTA
			,T10.N_NUM_VALORCOMERCIAL
			,T10.N_NUM_PRIMABRUTA
			,T10.N_NUM_PRIMANETA
			,T10.N_NUM_COM_CANAL
			,T10.N_NUM_VERSION
			,T10.C_OBS_DESCRIPTIVO
			,T10.N_ID_PERSONA
			,T10.C_NOM_COMPLETO
			,T10.N_ID_CONTRATANTE
			,T10.C_TIPO_IDENTIDAD_CONTRATANTE
			,T10.C_NUM_IDENTIDAD_CONTRATANTE
			,T10.C_NOM_COMPLETO_CONTRATANTE
			,T10.N_ID_UBIGEODEPARTAMENTO
			,T10.N_ID_UBIGEOPROVINCIA
			,T10.N_ID_UBIGEODISTRITO
			,T10.N_ID_TIPOVIA
			,T10.C_DES_DIRECCION
			,T10.C_VAL_NUMVIA
			,T10.N_ID_ASEGURADO
			,T10.N_ID_TIPO_IDENTIDAD_ASEGURADO
			,T10.C_TIPO_IDENTIDAD_ASEGURADO
			,T10.C_NUM_IDENTIDAD_ASEGURADO
			,T10.C_APE_PATERNO_ASEGURADO
			,T10.C_APE_MATERNO_ASEGURADO
			,T10.C_NOMBRE_ASEGURADO
			,T10.C_NOM_COMPLETO_ASEGURADO
			,T10.C_CELULAR_ASEGURADO
			,T10.C_CORREO_ASEGURADO
			,T10.N_ID_RESPONSABLEPAGO
			,T10.C_TIPO_IDENTIDAD_RESPONSABLEPAGO
			,T10.C_NUM_IDENTIDAD_RESPONSABLEPAGO
			,T10.C_NOM_COMPLETO_RESPONSABLEPAGO
			,T10.C_DES_TIPOMONEDA
			,T10.C_ABR_TIPOMONEDA
			,T10.N_IND_ACTIVO
			,T10.N_ID_ESTADO
			,T10.C_COD_USUCREACION
			,T10.D_FEC_CREACION
			,T10.C_COD_USUMODIF
			,T10.D_FEC_MODIFICACION
			,T10.N_ID_ESTADOTRANSFERENCIA
			,T10.C_DES_ESTADOTRANSFERENCIA
			,T10.MESHABILITADO
			,T10.N_ID_CANAL
			,T10.C_NOM_CANAL
			,T10.C_DES_REGION
			,T10.C_DES_ZONA
			,T10.N_ID_ESTABLECIMIENTO
			,T10.C_VAL_CODHOMOLOGACION
			,T10.C_NOM_ESTABLECIMIENTO
			,T10.N_ID_PEREMISOR
			,T10.C_NOM_COMPLETO_EMISOR
			,T10.N_ID_PERVENDEDOR
			,T10.C_VAL_CODREFERENCIA
			,T10.C_NOM_COMPLETO_VENDEDOR
			,T10.C_TIPO_FUNCIONARIO
			,T10.N_ID_ADJUNTOPOLIZA
			,T10.C_DES_ADJUNTOPOLIZA
			,T10.C_MODELO_SUBMODELO
			,T10.D_FEC_ANULACION
			,T10.N_ID_PERANULACION
			,PER.C_NOM_COMPLETO AS C_PER_ANULACION
			,T10.N_ID_MOTIVOANULA
			,T10.N_IND_ANULAR
			,T10.N_IND_CANCELAR
		FROM TABLA_10 T10
		LEFT JOIN T_MAE_PERSONA PER ON T10.N_ID_PERANULACION = PER.N_ID_PERSONA
	), TABLA_12 AS (
		SELECT
			T11.NUMROW
			,T11.ROW_TOTAL
			,T11.N_ID_POLIZA
			,T11.C_NUM_POLIZA
			,T11.N_ID_CERTIFICADO
			,T11.C_NUM_CERTIFICADO
			,T11.N_ID_COTIZACION
			,T11.C_NUM_COTIZACION
            ,T11.N_ID_SUSCRIPCION
			,T11.C_NUM_SOLICITUD
            ,T11.C_NUM_CREDITO
			,T11.N_ID_ENTIDAD
			,T11.C_NOM_ENTIDAD
			,T11.N_ID_RAMO
			,T11.N_ID_PRODUCTO
			,T11.C_DES_PRODUCTO
			,T11.N_ID_PLAN
			,T11.C_COD_PLAN
			,T11.C_DES_CORTAPLAN
			,T11.C_DES_LARGAPLAN
			,T11.C_COD_REFERENCIA
			,T11.C_COD_TRAMA
			,T11.C_VAL_SECPOLIZA
			,T11.C_VAL_POLIZA
			,T11.N_ID_TIPOPOLIZA
			,T11.N_ID_TIPOPAGOPOLIZA
			,T11.N_ID_TIPOMONEDA
			,T11.N_ID_TIPOREGLATARIFA
			,T11.N_ID_TIPOPLAN
			,T11.N_ID_TIPOVIGENCIA
			,T11.N_ID_TIPOCLIENTE
			,T11.N_ID_TIPOPERSONA
			,T11.D_FEC_CERTIFICADO
			,T11.D_FEC_INIVIGENCIA
			,T11.D_FEC_FINVIGENCIA
			,T11.D_FEC_REGISTRO
			,T11.D_FEC_VENTA
			,T11.N_NUM_VALORCOMERCIAL
			,T11.N_NUM_PRIMABRUTA
			,T11.N_NUM_PRIMANETA
			,T11.N_NUM_COM_CANAL
			,T11.N_NUM_VERSION
			,T11.C_OBS_DESCRIPTIVO
			,T11.N_ID_PERSONA
			,T11.C_NOM_COMPLETO
			,T11.N_ID_CONTRATANTE
			,T11.C_TIPO_IDENTIDAD_CONTRATANTE
			,T11.C_NUM_IDENTIDAD_CONTRATANTE
			,T11.C_NOM_COMPLETO_CONTRATANTE
			,T11.N_ID_UBIGEODEPARTAMENTO
			,T11.N_ID_UBIGEOPROVINCIA
			,T11.N_ID_UBIGEODISTRITO
			,T11.N_ID_TIPOVIA
			,T11.C_DES_DIRECCION
			,T11.C_VAL_NUMVIA
			,T11.N_ID_ASEGURADO
			,T11.N_ID_TIPO_IDENTIDAD_ASEGURADO
			,T11.C_TIPO_IDENTIDAD_ASEGURADO
			,T11.C_NUM_IDENTIDAD_ASEGURADO
			,T11.C_APE_PATERNO_ASEGURADO
			,T11.C_APE_MATERNO_ASEGURADO
			,T11.C_NOMBRE_ASEGURADO
			,T11.C_NOM_COMPLETO_ASEGURADO
			,T11.C_CELULAR_ASEGURADO
			,T11.C_CORREO_ASEGURADO
			,T11.N_ID_RESPONSABLEPAGO
			,T11.C_TIPO_IDENTIDAD_RESPONSABLEPAGO
			,T11.C_NUM_IDENTIDAD_RESPONSABLEPAGO
			,T11.C_NOM_COMPLETO_RESPONSABLEPAGO
			,T11.C_DES_TIPOMONEDA
			,T11.C_ABR_TIPOMONEDA
			,T11.C_DES_TIPOVIGENCIA
			,T11.N_IND_ACTIVO
			,T11.N_ID_ESTADO
			,T11.C_COD_USUCREACION
			,T11.D_FEC_CREACION
			,T11.C_COD_USUMODIF
			,T11.D_FEC_MODIFICACION
			,T11.N_ID_ESTADOTRANSFERENCIA
			,T11.C_DES_ESTADOTRANSFERENCIA
			,T11.MESHABILITADO
			,T11.N_ID_CANAL
			,T11.C_NOM_CANAL
			,T11.C_DES_REGION
			,T11.C_DES_ZONA
			,T11.N_ID_ESTABLECIMIENTO
			,T11.C_VAL_CODHOMOLOGACION
			,T11.C_NOM_ESTABLECIMIENTO
			,T11.N_ID_PEREMISOR
			,T11.C_NOM_COMPLETO_EMISOR
			,T11.N_ID_PERVENDEDOR
			,T11.C_VAL_CODREFERENCIA
			,T11.C_NOM_COMPLETO_VENDEDOR
			,T11.C_TIPO_FUNCIONARIO
			,T11.N_ID_ADJUNTOPOLIZA
			,T11.C_DES_ADJUNTOPOLIZA
			,T11.C_MODELO_SUBMODELO
			,T11.D_FEC_ANULACION
			,T11.N_ID_PERANULACION
			,T11.C_PER_ANULACION
			,MOA.C_DES_TIPO AS C_DES_MOTIVOANULACION
			,T11.N_IND_ANULAR
			,T11.N_IND_CANCELAR
		FROM TABLA_11 T11
		LEFT JOIN T_MAE_TIPO MOA ON T11.N_ID_MOTIVOANULA = MOA.N_ID_TIPO
	)

		SELECT			
			T12.NUMROW
			,T12.ROW_TOTAL
			,T12.N_ID_POLIZA
			,T12.C_NUM_POLIZA
			,T12.N_ID_CERTIFICADO
			,T12.C_NUM_CERTIFICADO
			,T12.N_ID_COTIZACION
			,T12.C_NUM_COTIZACION
            ,T12.N_ID_SUSCRIPCION
			,T12.C_NUM_SOLICITUD
            ,T12.C_NUM_CREDITO
			,T12.N_ID_ENTIDAD
			,T12.C_NOM_ENTIDAD
			,T12.N_ID_RAMO
			,T12.N_ID_PRODUCTO
			,T12.C_DES_PRODUCTO
			,T12.N_ID_PLAN
			,T12.C_COD_PLAN
			,T12.C_DES_CORTAPLAN
			,T12.C_DES_LARGAPLAN
			,T12.C_COD_REFERENCIA
			,T12.C_COD_TRAMA
			,T12.C_VAL_SECPOLIZA
			,T12.C_VAL_POLIZA
			,T12.N_ID_TIPOPOLIZA
			,T12.N_ID_TIPOPAGOPOLIZA
			,T12.N_ID_TIPOMONEDA
			,T12.N_ID_TIPOREGLATARIFA
			,T12.N_ID_TIPOPLAN
			,T12.N_ID_TIPOVIGENCIA
			,T12.N_ID_TIPOCLIENTE
			,T12.N_ID_TIPOPERSONA
			,T12.D_FEC_CERTIFICADO
			,T12.D_FEC_INIVIGENCIA
			,T12.D_FEC_FINVIGENCIA
			,T12.D_FEC_REGISTRO
			,T12.D_FEC_VENTA
			,T12.N_NUM_VALORCOMERCIAL
			,T12.N_NUM_PRIMABRUTA
			,T12.N_NUM_PRIMANETA
			,T12.N_NUM_COM_CANAL
			,T12.N_NUM_VERSION
			,T12.C_OBS_DESCRIPTIVO
			,T12.N_ID_PERSONA
			,T12.C_NOM_COMPLETO
			,T12.N_ID_CONTRATANTE
			,T12.C_TIPO_IDENTIDAD_CONTRATANTE
			,T12.C_NUM_IDENTIDAD_CONTRATANTE
			,T12.C_NOM_COMPLETO_CONTRATANTE
			,T12.N_ID_UBIGEODEPARTAMENTO
			,T12.N_ID_UBIGEOPROVINCIA
			,T12.N_ID_UBIGEODISTRITO
			,T12.N_ID_TIPOVIA
			,T12.C_DES_DIRECCION
			,T12.C_VAL_NUMVIA
			,T12.N_ID_ASEGURADO
			,T12.N_ID_TIPO_IDENTIDAD_ASEGURADO
			,T12.C_TIPO_IDENTIDAD_ASEGURADO
			,T12.C_NUM_IDENTIDAD_ASEGURADO
			,T12.C_APE_PATERNO_ASEGURADO
			,T12.C_APE_MATERNO_ASEGURADO
			,T12.C_NOMBRE_ASEGURADO
			,T12.C_NOM_COMPLETO_ASEGURADO
			,T12.C_CELULAR_ASEGURADO
			,T12.C_CORREO_ASEGURADO
			,T12.N_ID_RESPONSABLEPAGO
			,T12.C_TIPO_IDENTIDAD_RESPONSABLEPAGO
			,T12.C_NUM_IDENTIDAD_RESPONSABLEPAGO
			,T12.C_NOM_COMPLETO_RESPONSABLEPAGO
			,T12.C_DES_TIPOMONEDA
			,T12.C_ABR_TIPOMONEDA
			,T12.C_DES_TIPOVIGENCIA
			,T12.N_IND_ACTIVO
			,T12.N_ID_ESTADO
			,EST.C_DES_ESTADO
			,T12.C_COD_USUCREACION
			,T12.D_FEC_CREACION
			,T12.C_COD_USUMODIF
			,T12.D_FEC_MODIFICACION
			,T12.N_ID_ESTADOTRANSFERENCIA
			,T12.C_DES_ESTADOTRANSFERENCIA
			,T12.MESHABILITADO
			,T12.N_ID_CANAL
			,T12.C_NOM_CANAL
			,T12.C_DES_REGION
			,T12.C_DES_ZONA
			,T12.N_ID_ESTABLECIMIENTO
			,T12.C_VAL_CODHOMOLOGACION
			,T12.C_NOM_ESTABLECIMIENTO
			,T12.N_ID_PEREMISOR
			,T12.C_NOM_COMPLETO_EMISOR
			,T12.N_ID_PERVENDEDOR
			,T12.C_VAL_CODREFERENCIA
			,T12.C_NOM_COMPLETO_VENDEDOR
			,T12.C_TIPO_FUNCIONARIO
			,T12.N_ID_ADJUNTOPOLIZA
			,T12.C_DES_ADJUNTOPOLIZA
			,T12.C_MODELO_SUBMODELO
			,T12.D_FEC_ANULACION
			,T12.N_ID_PERANULACION
			,T12.C_PER_ANULACION
			,T12.C_DES_MOTIVOANULACION
			,T12.N_IND_ANULAR
			,T12.N_IND_CANCELAR
		FROM TABLA_12 T12
		INNER JOIN T_MAE_ESTADO EST ON T12.N_ID_ESTADO = EST.N_ID_ESTADO
		ORDER BY 1
	'
	print @script_ejecutar
	EXEC (@script_ejecutar)
END

GO