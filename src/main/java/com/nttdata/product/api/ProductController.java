package com.nttdata.product.api;

import com.nttdata.product.api.request.ProductRequest;
import com.nttdata.product.business.ProductService;
import com.nttdata.product.model.Product;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import jakarta.validation.constraints.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Mono;

@RestController
@RequestMapping(value = "/api/products")
public class ProductController {

    @Autowired
    private ProductService productService;

    /**
     * POST : Create a new product
     *
     * @param product (required)
     * @return Created (status code 201)
     */
    @Operation(
        operationId = "productsPost",
        summary = "Create a new product",
        responses = {
            @ApiResponse(responseCode = "201", description = "Created", content = {
                @Content(mediaType = "application/json", schema = @Schema(implementation = Product.class))
            })
        }
    )
    @PostMapping(
        value = "",
        produces = {"application/json"},
        consumes = {"application/json"}
    )
    public Mono<Product> productsPost(
        @Validated @RequestBody ProductRequest product
    ) {
        return productService.saveProduct(product);
    }

    /**
     * PUT : Update a product
     *
     * @param productId (required)
     * @param product   (required)
     * @return Created (status code 200)
     */
    @Operation(
        operationId = "productsPut",
        summary = "Update a product",
        responses = {
            @ApiResponse(responseCode = "200", description = "Updated", content = {
                @Content(mediaType = "application/json", schema = @Schema())
            })
        }
    )
    @PutMapping(
        value = "/{productId}",
        produces = {"application/json"},
        consumes = {"application/json"}
    )
    public Mono<Product> productsPut(
        @Parameter(name = "productId", description = "", required = true, in = ParameterIn.PATH)
        @PathVariable("productId") String productId,
        @Validated @RequestBody ProductRequest product
    ) {
        return productService.updateProduct(product, productId);
    }

    /**
     * GET : Get a product
     *
     * @param productType (required)
     * @return OK (status code 200)
     */
    @Operation(
        operationId = "productsGet",
        summary = "Get a product",
        responses = {
            @ApiResponse(responseCode = "200", description = "OK", content = {
                @Content(mediaType = "application/json",
                    array = @ArraySchema(schema = @Schema(implementation = Product.class)))
            })
        }
    )
    @GetMapping(
        value = "",
        produces = {"application/json"}
    )
    public Mono<Product> productsGet(
        @NotNull @Parameter(name = "productType", description = "", required = true, in = ParameterIn.QUERY)
        @Validated @RequestParam(value = "productType") String productType
    ) {
        return productService.getProduct(productType);
    }

    /**
     * DELETE : delete a product
     *
     * @param productId (required)
     * @return Created (status code 200)
     */
    @Operation(
        operationId = "productDelete",
        summary = "Delete a product",
        responses = {
            @ApiResponse(responseCode = "200", description = "Deleted")
        }
    )
    @DeleteMapping(value = "/{productId}")
    public Mono<Void> productDelete(
        @Parameter(name = "productId", description = "", required = true, in = ParameterIn.PATH)
        @PathVariable("productId") String productId
    ) {
        return productService.deleteProduct(productId);
    }

}

